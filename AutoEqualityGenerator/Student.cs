using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Text;

namespace AutoEqualityGenerator
{

    [AutoEquality]
    public partial class Student
    {
        public string FirstName { get; set; } = "";
        public string LastName { get; set; } = "";

        public Student(string firstName, string lastName)
        {
            FirstName = firstName;
            LastName = lastName;
        }
    }
    [AutoEquality(true)]
    public sealed partial class Student2 : Student
    {
        public double Size { get; set; }

        public Student Student { get; set; }

        public Student2(string firstName, string lastName) : base(firstName, lastName)
        {
        }
    }
}
