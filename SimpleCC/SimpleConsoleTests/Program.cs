﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Parser;
using XPath2.Parser;
using SimpleRegexIntersector;

namespace SimpleConsoleTests
{
   class Program
   {
      static void Main(String[] args)
      {
         // (a | b) b

         String tInput;
         while((tInput = Console.ReadLine()) != "\\quit")
            try
            {
               Console.WriteLine("Parsed: " + Regex.Parse(tInput));
            }
            catch (Exception e)
            {
               Console.WriteLine("ERROR: " + e.Message);
            }


         //Regex tAnyZ = new KleeneRegex() { Operand = new LetterRegex() { Letter = 'z' } };
         //Regex tChoice = new ChoiceRegex() { Left = tAnyZ, Right = new LetterRegex() { Letter = 'a' } };

         //Regex tOne = new SeqRegex() { Left = tChoice, Right = new LetterRegex() { Letter = 'c' } };
         //Regex tTwo = new SeqRegex() { Left = new LetterRegex() { Letter = 'a' }, Right = new LetterRegex() { Letter = 'b' } };

         //Console.WriteLine("One: " + tOne);
         //Console.WriteLine("Two: " + tTwo);

         //Console.WriteLine("Equal: " + tOne.SemanticEquals(tTwo));
         //Console.WriteLine("Intersect: " + tOne.Intersects(tTwo));
         //Console.WriteLine("Intersection: " + tOne.Intersect(new Dictionary<Pair, Regex>(), 0, tTwo));

         //Console.Read();
      }

      class Foo { public Int32 Bar = 1; }

      static void Main2(String[] args)
      {
         Foo foo = new Foo();
         Action a = () => { Console.WriteLine(foo.Bar); };
         a();
         List<Foo> list = new List<Foo>();
         list.Add(foo);
         Two(list);
         a();
         Console.Read();
      }

      static void Two(ref Foo foo) { foo.Bar = 2; }
      static void Two(List<Foo> list) { list[0].Bar = 2; }

      static void Main3(string[] args)
      {
         for(;;)
         {
            Console.Write(">");
            try
            {
               String tLine = Console.ReadLine();
               if (tLine == "!")
                  return;

               XPath2Parser tParser = new XPath2Parser();
               tParser.Parse(tLine);
               Console.WriteLine("OK");
            }
            catch (Exception e)
            {
               Console.WriteLine("ERROR: " + e.Message);
         //      Console.WriteLine(e.StackTrace);
            }
         }
      }
   }
}
