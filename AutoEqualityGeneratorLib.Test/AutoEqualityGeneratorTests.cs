using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using System;

namespace Generators.Tests
{
    public sealed class AutoEqualityGeneratorTests
    {
        [Fact]
        public void GeneratesEqualityForClass_WithAutoEqualityAttribute()
        {
            // Arrange
            var inputCode = @"
                using System;
                [AutoEquality]
                public partial class Person
                {
                    public string Name { get; set; }
                    public int Age { get; set; }
                }
            ";

            // Act
            var generatedCode = RunGenerator(inputCode);

            // Assert
            Assert.Contains("partial class Person : IEquatable<Person>", generatedCode);
            Assert.Contains("public bool Equals(Person? other)", generatedCode);
            Assert.Contains("public override int GetHashCode()", generatedCode);
        }

        [Fact]
        public void GeneratesEqualityForStruct_WithAutoEqualityAttribute()
        {
            // Arrange: Le code source qui sera donn� en entr�e au g�n�rateur
            var inputCode = @"
                using System;

                [AutoEquality]
                public partial struct Point
                {
                    public int X { get; set; }
                    public int Y { get; set; }
                }
            ";

            // Act: Ex�cution du g�n�rateur sur le code source
            var generatedCode = RunGenerator(inputCode);

            // Assert: V�rifie si le g�n�rateur produit la m�thode Equals et GetHashCode pour un struct
            Assert.Contains("partial struct Point : IEquatable<Point>", generatedCode);
            Assert.Contains("public bool Equals(Point other)", generatedCode);
            Assert.Contains("public override int GetHashCode()", generatedCode);
        }

        [Fact]
        public Task GeneratesCaseInsensitiveEquality_ForStringProperties()
        {
            // Arrange: Le code source avec l'attribut `AutoEquality(caseInsensitive: true)`
            var inputCode = @"
                using System;

               [AutoEquality(CaseInsensitive = true)]
                public sealed partial class CaseInsensitivePerson
                {
                    public string Name { get; set; }
                    public int Age { get; set; }
                }

                [AutoEquality(CaseInsensitive = true)]
                public sealed partial class CaseInsensitivePerson2
                {
                    public string Name { get; set; }
                    public int Age { get; set; }
                }
            ";

            // Act: Ex�cution du g�n�rateur sur le code source
            var generatedCode = RunGenerator(inputCode);
            
            // Assert
            return Verify(generatedCode);
        }

        [Fact]
        public Task DoesNotGenerateEquality_WhenAttributeIsMissing()
        {
            // Arrange: Code sans attribut `AutoEquality`
            var inputCode = @"
                public class NoEquality
                {
                    public string Name { get; set; }
                    public int Age { get; set; }
                }
            ";

            // Act: Ex�cution du g�n�rateur sur le code source
            var generatedCode = RunGenerator(inputCode);

            // Assert: V�rifie qu'aucun code n'est g�n�r� si l'attribut est absent
            Assert.DoesNotContain("IEquatable", generatedCode);

            // Assert
            return Verify(generatedCode);
        }

        private string RunGenerator(string sourceCode)
        {
            // Cr�er un compilateur Roslyn
            var syntaxTree = CSharpSyntaxTree.ParseText(sourceCode);
            var compilation = CSharpCompilation.Create("TestCompilation",
                new[] { syntaxTree },
                new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) },
                new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary));

            // Initialiser et ex�cuter le g�n�rateur
            var generator = new AutoEqualityGenerator();
            GeneratorDriver driver = CSharpGeneratorDriver.Create(generator);

            driver = driver.RunGenerators(compilation);

            GeneratorDriverRunResult runResult = driver.GetRunResult();
            var generatedSource = runResult.Results[0].GeneratedSources;

            // Retourner le code g�n�r� sous forme de cha�ne
            return generatedSource.Length > 1 ? generatedSource[1].SourceText.ToString() : string.Empty;
        }

    }
}
