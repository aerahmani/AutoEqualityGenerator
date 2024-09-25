using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using System;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Text;

namespace Generators
{
    [Generator]
    public class AutoEqualityGenerator : IIncrementalGenerator
    {
        private const string attributeText = @"
using System;
[AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, Inherited = false, AllowMultiple = false)]
internal sealed class AutoEqualityAttribute : Attribute
{
    public bool CaseInsensitive { get; set; }

    public AutoEqualityAttribute(bool caseInsensitive = false) =>
        CaseInsensitive = caseInsensitive;
}
";
        /// <summary>
        /// Initializes the generator and registers the necessary actions for source generation.
        /// </summary>
        /// <param name="context">The context for incremental source generation initialization.</param>
        /// <remarks>
        /// This method performs the following tasks:
        /// 1. Registers the AutoEqualityAttribute as a generated source.
        /// 2. Sets up a syntax provider to identify potential targets for code generation.
        /// 3. Combines the compilation with the collected type declarations.
        /// 4. Registers the source output generation using the Execute method.
        /// </remarks>

        public void Initialize(IncrementalGeneratorInitializationContext context)
        {
            // Add source attribute for all compilations
            context.RegisterPostInitializationOutput(ctx => ctx.AddSource("AutoEqualityAttribute.g.cs", SourceText.From(attributeText, Encoding.UTF8)));

            // Create an IncrementalValueProvider that extracts type declarations with AutoEqualityAttribute
            IncrementalValuesProvider<TypeDeclarationSyntax> typeDeclarations = context.SyntaxProvider
                .CreateSyntaxProvider(
                    predicate: static (s, _) => IsSyntaxTargetForGeneration(s),
                    transform: static (ctx, _) => GetSemanticTargetForGeneration(ctx))
                .Where(static m => m is not null)!;

            // Generate code for each type declaration found
            IncrementalValueProvider<(Compilation, ImmutableArray<TypeDeclarationSyntax>)> compilationAndTypes = context.CompilationProvider.Combine(typeDeclarations.Collect());


            context.RegisterSourceOutput(compilationAndTypes, (spc, source) => Execute(source.Item1, source.Item2, spc));
        }

        private static bool IsSyntaxTargetForGeneration(SyntaxNode node)
        {
            // Select classes or structs that have an attribute
            return node is TypeDeclarationSyntax { AttributeLists.Count: > 0 };
        }

        private static TypeDeclarationSyntax? GetSemanticTargetForGeneration(GeneratorSyntaxContext context)
        {
            var typeDeclarationSyntax = (TypeDeclarationSyntax)context.Node;

            // Search for the "AutoEquality" attribute
            foreach (var attributeList in typeDeclarationSyntax.AttributeLists)
            {
                foreach (var attribute in attributeList.Attributes)
                {
                    if (context.SemanticModel.GetSymbolInfo(attribute).Symbol is IMethodSymbol attributeSymbol &&
                        attributeSymbol.ContainingType.ToDisplayString() == "AutoEqualityAttribute")
                    {
                        return typeDeclarationSyntax;
                    }
                }
            }

            // If no matching attribute, we ignore
            return null;
        }

        private void Execute(Compilation compilation, ImmutableArray<TypeDeclarationSyntax> types, SourceProductionContext context)
        {
            if (types.IsDefaultOrEmpty)
                return;

            var builder = new StringBuilder();
            var list = new List<(INamedTypeSymbol NamedTypeSymbol, bool IsAnnotated, bool IsCaseInsensitive)>();

            foreach (var typeSyntax in types.Distinct())
            {
                var semanticModel = compilation.GetSemanticModel(typeSyntax.SyntaxTree);
                INamedTypeSymbol? namedTypeSymbolusage = null;
                if (semanticModel.GetDeclaredSymbol(typeSyntax) is INamedTypeSymbol namedTypeSymbol)
                {
                    namedTypeSymbolusage = namedTypeSymbol;
                    var isAnnotated = semanticModel.GetNullableContext(typeSyntax.SpanStart) == NullableContext.Enabled;

                    // Check if the InEquality attribute is marked as "Case Insensitive"
                    var autoEqualityAttribute = namedTypeSymbol.GetAttributes()
                        .FirstOrDefault(attr => attr.AttributeClass?.ToDisplayString() == "AutoEqualityAttribute");
                    var isCaseInsensitive = autoEqualityAttribute?.NamedArguments
                        .FirstOrDefault(arg => arg.Key == "CaseInsensitive")
                        .Value.Value as bool? ?? false;

                    list.Add((namedTypeSymbol, isAnnotated, isCaseInsensitive));
                }
            }

            if (list.Any())
            {

                foreach (var item in list)
                {
                    AddTypeGeneration(builder, item);
                    context.AddSource($"{item.NamedTypeSymbol.Name}.g.cs", SourceText.From(builder.ToString(), Encoding.UTF8));
                    builder.Clear();
                }
                list.Clear();
            }




        }

        private void AddTypeGeneration(
           StringBuilder builder,
           (INamedTypeSymbol NamedTypeSymbol, bool IsAnnotated, bool IsCaseInsensitive) typeSymbol)
        {

            var indent = new IndentUtil();
            builder.AppendLine($@"
using System;
using System.Collections.Generic;");

            var namespaceSymbol = typeSymbol.NamedTypeSymbol.ContainingNamespace;
            if (!namespaceSymbol.IsGlobalNamespace)
            {
                builder.AppendLine($@"namespace {namespaceSymbol}
{{");
                indent.IncreaseSimple();
            }

            AddTypeGeneration(builder, indent, typeSymbol.NamedTypeSymbol, typeSymbol.IsAnnotated, typeSymbol.IsCaseInsensitive);

            if (!namespaceSymbol.IsGlobalNamespace)
            {
                indent.Decrease();
                builder.AppendLine("}");
            }
        }

      

        private void AddTypeGeneration(
            StringBuilder builder, IndentUtil indent, INamedTypeSymbol typeSymbol,
            bool isAnnotated, bool isCaseInsensitive)
        {
            var kind = typeSymbol.TypeKind == TypeKind.Class ? "class" : "struct";

            var refAnnotation = isAnnotated ? "?" : "";
            var typeAnnotation = typeSymbol.TypeKind == TypeKind.Class ? "?" : "";


            builder.AppendLine($@"

{indent.Value}#nullable enable
{indent.Value}partial {kind} {typeSymbol.Name} : IEquatable<{typeSymbol.Name}>
{indent.Value}{{
{indent.Value2}public override bool Equals(object{refAnnotation} obj) => obj is {typeSymbol.Name} other && Equals(other);");

            AddOperatorEquals();
            var memberInfoList = GetMemberInfo(typeSymbol);
            using var marker = indent.Increase();

            AddEquals();
            AddGetHashCode();

            marker.Revert();
            builder.AppendLine($@"{indent.Value}}}");

            void AddOperatorEquals()
            {
                using var _ = indent.Increase();
                var prefix = !typeSymbol.IsValueType ? "left is object && " : "";
                builder.AppendLine($"{indent.Value}public static bool operator==({typeSymbol.Name}{typeAnnotation} left, {typeSymbol.Name}{typeAnnotation} right) => {prefix}left.Equals(right);");
                builder.AppendLine($"{indent.Value}public static bool operator!=({typeSymbol.Name}{typeAnnotation} left, {typeSymbol.Name}{typeAnnotation} right) => !(left == right);");
            }

            void AddEquals()
            {
                builder.AppendLine($@"
{indent.Value}public bool Equals({typeSymbol.Name}{typeAnnotation} other)
{indent.Value}{{
{indent.Value2}return");

                using var marker = indent.Increase(2);

                if (typeSymbol.TypeKind == TypeKind.Class)
                {
                    builder.AppendLine($"{indent.Value}other is object &&");
                }

                for (var i = 0; i < memberInfoList.Count; i++)
                {
                    MemberInfo member = memberInfoList[i];
                    var line = (
                        member.UseOperator,
                        member.IsString) switch
                    {
                        (true, _) => $"{indent.Value}{member.Name} == other.{member.Name}",
                        (_, true) => isCaseInsensitive
                            ? $"{indent.Value}string.Equals({member.Name}, other.{member.Name}, StringComparison.OrdinalIgnoreCase)"
                            : $"{indent.Value}string.Equals({member.Name}, other.{member.Name})",
                        _ => $"{indent.Value}EqualityComparer<{member.TypeName}>.Default.Equals({member.Name}, other.{member.Name})"
                    };

                    builder.Append(line);
                    builder.Append(i + 1 < memberInfoList.Count ? " &&" : ";");
                    builder.AppendLine();
                }

                marker.Revert();
                builder.AppendLine($"{indent.Value}}}");

            }

            void AddGetHashCode()
            {
                builder.AppendLine($@"
{indent.Value}public override int GetHashCode()
{indent.Value}{{
{indent.Value2}var hash = new HashCode();");

                for (var i = 0; i < memberInfoList.Count; i++)
                {
                    var current = memberInfoList[i];
                    builder.AppendLine($"{indent.Value2}hash.Add({current.Name});");
                }

                builder.AppendLine($"{indent.Value2}return hash.ToHashCode();");
                builder.AppendLine($"{indent.Value}}}");
            }
            builder.AppendLine("#nullable disable");
        }

        private List<MemberInfo> GetMemberInfo(INamedTypeSymbol typeSymbol)
        {
            var list = new List<MemberInfo>();
            foreach (var symbol in typeSymbol.GetMembers())
            {
                switch (symbol)
                {
                    case IFieldSymbol { Type: { }, IsImplicitlyDeclared: false } fieldSymbol:
                        list.Add(new MemberInfo(fieldSymbol.Name, fieldSymbol.Type.ToDisplayString(), UseOperator(fieldSymbol.Type), IsString(fieldSymbol.Type)));
                        break;
                    case IPropertySymbol { IsIndexer: false, GetMethod: { } } propertySymbol:
                        list.Add(new MemberInfo(propertySymbol.Name, propertySymbol.Type.ToDisplayString(), UseOperator(propertySymbol.Type), IsString(propertySymbol.Type)));
                        break;
                }
            }
            return list;
        }

        private record MemberInfo
        {
            public MemberInfo(string name, string typeName, bool useOperator, bool isString)
            {
                Name = name;
                TypeName = typeName;
                UseOperator = useOperator;
                IsString = isString;
            }
            public string Name { get; set; }
            public string TypeName { get; set; }
            public bool UseOperator { get; set; }
            public bool IsString { get; set; }
        }

        private static bool UseOperator(ITypeSymbol? type) =>
                    type is
                    {
                        SpecialType:
                            SpecialType.System_Int16 or
                            SpecialType.System_Int32 or
                            SpecialType.System_Int64 or
                            SpecialType.System_UInt16 or
                            SpecialType.System_UInt32 or
                            SpecialType.System_UInt64 or
                            SpecialType.System_IntPtr or
                            SpecialType.System_UIntPtr
                    };

        private static bool IsString(ITypeSymbol? type) =>
                    type is { SpecialType: SpecialType.System_String };
    }
}
