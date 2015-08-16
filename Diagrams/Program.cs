using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.MSBuild;
using Microsoft.CodeAnalysis.FindSymbols;

namespace DotNetDiagrams
{
    /// <summary>
    /// this is a proof of concept brute-force approach to get familiar with Roslyn
    /// - the Wiki provides details on usage: https://github.com/SoundLogic/Diagrams/wiki
    /// 
    /// For interested parties:
    /// Due to some of the limitations of Roslyn (the nature of analyzing IL - lambdas/iterator 
    /// methods/await | accessing code in external DLLs ), I did not continue
    /// to pursue / formalize this project - it would likely be easier to use a static analysis 
    /// library like Mono.Cecil
    /// </summary>
    class Program
    {
        static void Main(string[] args)
        {
            const string solutionName = "DotNetDiagrams";
            const string solutionExtension = ".sln";
            const string solutionFileName = solutionName + solutionExtension;
            const string rootPath = @"C:\Workspace\";
            const string solutionPath = rootPath + solutionName + @"\" + solutionFileName;

            var workspace = MSBuildWorkspace.Create();
            var diagramGenerator = new DiagramGenerator(solutionPath, workspace);
            diagramGenerator.ProcessSolution().Wait();
            diagramGenerator.GenerateDiagramFromRoot();
            Console.ReadKey();
        }
    }

    class DiagramGenerator
    {
        #region [Fields & Properties]
        private readonly Solution _solution;

        private readonly ConcurrentDictionary<MethodDeclarationSyntax, List<MethodDeclarationSyntax>> _methodDeclarationSyntaxes
            = new ConcurrentDictionary<MethodDeclarationSyntax, List<MethodDeclarationSyntax>>();

        private readonly ConcurrentDictionary<MethodDeclarationSyntax, Dictionary<int, MethodDeclarationSyntax>> _methodOrder
            = new ConcurrentDictionary<MethodDeclarationSyntax, Dictionary<int, MethodDeclarationSyntax>>();
        #endregion

        #region [Constructor]
        public DiagramGenerator(string solutionPath, MSBuildWorkspace workspace)
        {
            _solution = workspace.OpenSolutionAsync(solutionPath).Result;
        }
        #endregion

        #region [process the tree]
        private async Task ProcessCompilation(Compilation compilation)
        {
            var trees = compilation.SyntaxTrees;

            foreach (var tree in trees)
            {
                var root = await tree.GetRootAsync();
                var classes = root.DescendantNodes().OfType<ClassDeclarationSyntax>();
                SyntaxTree treeCopy = tree;
                foreach (var @class in classes)
                {
                    await ProcessClass(@class, compilation, treeCopy);
                }
            }
        }

        private async Task ProcessClass(
            ClassDeclarationSyntax @class
            , Compilation compilation
            , SyntaxTree syntaxTree)
        {
            var methods = @class.DescendantNodes().OfType<MethodDeclarationSyntax>();
            foreach (var method in methods)
            {
                await ProcessMethod(method, compilation, syntaxTree);
            }
        }

        private async Task ProcessMethod(
              MethodDeclarationSyntax method
            , Compilation compilation
            , SyntaxTree syntaxTree)
        {
            var model = compilation.GetSemanticModel(syntaxTree);

            var methodSymbol = model.GetDeclaredSymbol(method);

            var callingMethods = await GetCallingMethodsAsync(methodSymbol, method);

            Parallel.ForEach(callingMethods, callingMethod =>
            {
                ClassDeclarationSyntax callingClass = null;
                if (SyntaxNodeHelper.TryGetParentSyntax(method, out callingClass))
                {
                    List<MethodDeclarationSyntax> value;
                    if (!_methodDeclarationSyntaxes.TryGetValue(callingMethod, out value))
                    {
                        if (!_methodDeclarationSyntaxes.TryAdd(callingMethod, new List<MethodDeclarationSyntax>() { method }))
                        {
                            throw new Exception("Could not add item to _methodDeclarationSyntaxes!");
                        }
                    }
                    else
                    {
                        value.Add(method);
                    }
                }
            });
        }

        /// <summary>
        /// Gets a list of methods that call the method based on the method symbol
        /// also builds a list of called methods by the calling method as the key and then the value is a dictionary 
        /// of UInt64,MethodDeclarationSyntax where the UInt64 is the start location of the span where the called method is called
        /// from inside the calling method - this will allow us to order our sequence diagrams, but this functionality should be moved out into a separate method at some point
        /// (in fact this whole method needs a ton of refactoring and is too complex)
        /// </summary>
        /// <param name="methodSymbol"></param>
        /// <param name="method"></param>
        /// <returns></returns>
        private async Task<List<MethodDeclarationSyntax>> GetCallingMethodsAsync(IMethodSymbol methodSymbol, MethodDeclarationSyntax method)
        {
            var references = new List<MethodDeclarationSyntax>();

            var referencingSymbols = await SymbolFinder.FindCallersAsync(methodSymbol, _solution);
            var referencingSymbolsList = referencingSymbols as IList<SymbolCallerInfo> ?? referencingSymbols.ToList();

            if (!referencingSymbolsList.Any(s => s.Locations.Any()))
            {
                return references;
            }

            foreach (var referenceSymbol in referencingSymbolsList)
            {
                foreach (var location in referenceSymbol.Locations)
                {
                    var position = location.SourceSpan.Start;
                    var root = await location.SourceTree.GetRootAsync();
                    var nodes = root.FindToken(position).Parent.AncestorsAndSelf().OfType<MethodDeclarationSyntax>();

                    var methodDeclarationSyntaxes = nodes as MethodDeclarationSyntax[] ?? nodes.ToArray();
                    references.AddRange(methodDeclarationSyntaxes);

                    // we need to know what order methods are called in
                    foreach (var methodCall in methodDeclarationSyntaxes)
                    {
                        Dictionary<int, MethodDeclarationSyntax> value;

                        if (!_methodOrder.TryGetValue(methodCall, out value))
                        {
                            var dictionary = new Dictionary<int, MethodDeclarationSyntax>();
                            dictionary.Add(location.SourceSpan.Start, method);

                            if (!_methodOrder.TryAdd(methodCall, dictionary))
                            {
                                throw new Exception("Could not add item to _methodOrder!");
                            }
                        }
                        else
                        {
                            value.Add(location.SourceSpan.Start, method);
                        }
                    }
                }
            }

            return references;
        }
        #endregion

        #region [42]
        // I was drunk when I wrote this!
        // ... that's not true .. just trying to follow proper open source protocol
        #endregion

        #region [build & output js-sequence-diagrams formatted text]

        /// <summary>
        /// generates diagram by order of methods getting called based on the first method found that does not have anything calling it
        /// </summary>
        public void GenerateDiagramFromRoot()
        {
            MethodDeclarationSyntax root = null;
            foreach (var key in _methodDeclarationSyntaxes.Keys)
            {
                if (!_methodDeclarationSyntaxes.Values.Any(value => value.Contains(key)))
                {
                    // then we have a method that's not being called by anything
                    root = key;
                    break;
                }
            }

            if (root != null)
            {
                PrintMethodInfo(root);
            }
        }

        public void PrintMethodInfo(MethodDeclarationSyntax callingMethod)
        {
            if (!_methodDeclarationSyntaxes.ContainsKey(callingMethod))
            {
                return;
            }

            var calledMethods = _methodOrder[callingMethod];
            var orderedCalledMethods = calledMethods.OrderBy(kvp => kvp.Key);

            foreach (var kvp in orderedCalledMethods)
            {
                var calledMethod = kvp.Value;
                ClassDeclarationSyntax callingClass = null;
                ClassDeclarationSyntax calledClass = null;

                if (!SyntaxNodeHelper.TryGetParentSyntax(callingMethod, out callingClass) ||
                    !SyntaxNodeHelper.TryGetParentSyntax(calledMethod, out calledClass))
                {
                    continue;
                }

                PrintOutgoingCallInfo(
                          calledClass
                        , callingClass
                        , callingMethod
                        , calledMethod
                    );

                if (callingMethod != calledMethod)
                {
                    PrintMethodInfo(calledMethod);
                }

                PrintReturnCallInfo(
                          calledClass
                        , callingClass
                        , callingMethod
                        , calledMethod
                    );
            }
        }

        private static void PrintOutgoingCallInfo(
              ClassDeclarationSyntax classBeingCalled
            , ClassDeclarationSyntax callingClass
            , MethodDeclarationSyntax callingMethod
            , MethodDeclarationSyntax calledMethod
            , bool includeCalledMethodArguments = false)
        {
            var callingMethodName = callingMethod.Identifier.ToFullString();
            var calledMethodReturnType = calledMethod.ReturnType.ToFullString();
            var calledMethodName = calledMethod.Identifier.ToFullString();
            var calledMethodArguments = calledMethod.ParameterList.ToFullString();
            var calledMethodModifiers = calledMethod.Modifiers.ToString();
            var calledMethodConstraints = calledMethod.ConstraintClauses.ToFullString();
            var actedUpon = classBeingCalled.Identifier.ValueText;
            var actor = callingClass.Identifier.ValueText;
            var calledMethodTypeParameters = calledMethod.TypeParameterList != null
                ? calledMethod.TypeParameterList.ToFullString()
                : String.Empty;
            var callingMethodTypeParameters = callingMethod.TypeParameterList != null
                ? callingMethod.TypeParameterList.ToFullString()
                : String.Empty;

            var callInfo = callingMethodName + callingMethodTypeParameters + " => " + calledMethodModifiers + " " + calledMethodReturnType + calledMethodName + calledMethodTypeParameters;

            if (includeCalledMethodArguments)
            {
                callInfo += calledMethodArguments;
            }

            callInfo += calledMethodConstraints;

            string info
                = BuildOutgoingCallInfo(actor
                                        , actedUpon
                                        , callInfo);

            Console.Write(info);
        }

        private static void PrintReturnCallInfo(
              ClassDeclarationSyntax classBeingCalled
            , ClassDeclarationSyntax callingClass
            , MethodDeclarationSyntax callingMethod
            , MethodDeclarationSyntax calledMethod)
        {

            var actedUpon = classBeingCalled.Identifier.ValueText;
            var actor = callingClass.Identifier.ValueText;
            var callerName = callingMethod.Identifier.ToFullString();
            var callingMethodTypeParameters = callingMethod.TypeParameterList != null
                ? callingMethod.TypeParameterList.ToFullString()
                : String.Empty;
            var calledMethodTypeParameters = calledMethod.TypeParameterList != null
                ? calledMethod.TypeParameterList.ToFullString()
                : String.Empty;

            var calledMethodInfo = calledMethod.Identifier.ToFullString() + calledMethodTypeParameters;

            callerName += callingMethodTypeParameters;

            var returnCallInfo = calledMethod.ReturnType.ToString();

            var returnMethodParameters = calledMethod.ParameterList.Parameters;
            foreach (var parameter in returnMethodParameters)
            {
                if (parameter.Modifiers.Any(m => m.Text == "out"))
                {
                    returnCallInfo += "," + parameter.ToFullString();
                }
            }

            string info = BuildReturnCallInfo(
                  actor
                , actedUpon
                , calledMethodInfo
                , callerName
                , returnCallInfo);

            Console.Write(info);
        }

        private static string BuildOutgoingCallInfo(string actor, string actedUpon, string callInfo)
        {
            const string calls = "->";
            const string descriptionSeparator = ": ";

            string callingInfo = actor + calls + actedUpon + descriptionSeparator + callInfo;

            callingInfo = callingInfo.RemoveNewLines(true);

            string result = callingInfo + Environment.NewLine;

            return result;
        }

        private static string BuildReturnCallInfo(string actor, string actedUpon, string calledMethodInfo, string callerName, string returnInfo)
        {
            const string returns = "-->";
            const string descriptionSeparator = ": ";

            string returningInfo = actedUpon + returns + actor + descriptionSeparator + calledMethodInfo + " returns " + returnInfo + " to " + callerName;
            returningInfo = returningInfo.RemoveNewLines(true);

            string result = returningInfo + Environment.NewLine;

            return result;
        }

        public async Task ProcessSolution()
        {
            foreach (Project project in _solution.Projects)
            {
                Compilation compilation = await project.GetCompilationAsync();
                await ProcessCompilation(compilation);
            }
        }

        #endregion
    }

    static class SyntaxNodeHelper
    {
        public static bool TryGetParentSyntax<T>(SyntaxNode syntaxNode, out T result)
            where T : SyntaxNode
        {
            // set defaults
            result = null;

            if (syntaxNode == null)
            {
                return false;
            }

            try
            {
                syntaxNode = syntaxNode.Parent;

                if (syntaxNode == null)
                {
                    return false;
                }

                if (syntaxNode.GetType() == typeof(T))
                {
                    result = syntaxNode as T;
                    return true;
                }

                return TryGetParentSyntax<T>(syntaxNode, out result);
            }
            catch
            {
                return false;
            }
        }
    }

    public static class StringEx
    {
        public static string RemoveNewLines(this string stringWithNewLines, bool cleanWhitespace = false)
        {
            string stringWithoutNewLines = null;
            List<char> splitElementList = Environment.NewLine.ToCharArray().ToList();

            if (cleanWhitespace)
            {
                splitElementList.AddRange(" ".ToCharArray().ToList());
            }

            char[] splitElements = splitElementList.ToArray();

            var stringElements = stringWithNewLines.Split(splitElements, StringSplitOptions.RemoveEmptyEntries);
            if (stringElements.Any())
            {
                stringWithoutNewLines = stringElements.Aggregate(stringWithoutNewLines, (current, element) => current + (current == null ? element : " " + element));
            }

            return stringWithoutNewLines ?? stringWithNewLines;
        }
    }
}
