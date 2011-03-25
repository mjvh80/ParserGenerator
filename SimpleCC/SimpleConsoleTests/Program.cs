using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using SimpleCC;
using XPath2.Parser;
using SimpleRegexIntersector;

namespace SimpleConsoleTests
{
   class Program
   {
      static Boolean ReadLine(ref String line)
      {
         return (line = Console.ReadLine()) != "\\q";
      }

      static void Main3(String[] args)
      {
         // (a | b) b

         String tInput = "";
         for (; ; )
            try
            {
               SimpleRegex tLeft, tRight, tExpectedPrefix;
               Console.Write("Enter regex 1: ");
               if (!ReadLine(ref tInput))
                  return;
               Console.WriteLine("Parsed: " + (tLeft = SimpleRegex.Parse(tInput)));

               Console.Write("Enter regex 2: ");
               if (!ReadLine(ref tInput))
                  return;
               Console.WriteLine("Parsed: " + (tRight = SimpleRegex.Parse(tInput)));

               Console.Write("Enter expected prefix (no rewrite): ");
               if (!ReadLine(ref tInput))
                  return;
               Console.WriteLine("Parsed: " + (tExpectedPrefix = SimpleRegex.Parse(tInput)));

               // todo: this is not the way we want to do the intersection
               // Rewrite was made internal..
               SimpleRegex tRewrite = null; // new ChoiceRegex() { Left = tLeft, Right = tRight }.Rewrite();

               // split up:
               tLeft = ((ChoiceRegex)tRewrite).Left;
               tRight = ((ChoiceRegex)tRewrite).Right;

               Console.WriteLine("Rewritten (1) hashcode: " + tLeft.EqualsConsistentHashCode() + " " + tLeft);
               Console.WriteLine("Rewritten (2) hashcode: " + tRight.EqualsConsistentHashCode() + " " + tRight);

               Console.WriteLine("Equal: " + tLeft.SemanticEquals(tRight));
               // Commented out, as the access of these methods has changed
               //Console.WriteLine("Matches exp. prefix: " + tLeft.GetCommonPrefix(new Dictionary<Pair, SimpleRegex>(), 0, tRight).SharesCommonPrefixWith(tExpectedPrefix));
               // commented out becaues Intersect is now internal..
               //         Console.WriteLine("Intersect: " + tLeft.Intersects(tRight) + " - intersection is " + tLeft.Intersect(new Dictionary<Pair, SimpleRegex>(), 0, tRight));
               //Console.WriteLine("Prefix intersect: " + tLeft.SharesCommonPrefixWith(tRight) + " - prefix intersection is " + tLeft.GetCommonPrefix(new Dictionary<Pair, SimpleRegex>(), 0, tRight));

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

      static void Main(string[] args)
      {
         ParserBase tParser = null;
         System.Diagnostics.Stopwatch tTimer = null;
         try
         {
            Console.Write("Generating parser...");
            tTimer = System.Diagnostics.Stopwatch.StartNew();
            tParser = new XPath2Parser().Build();
            Console.WriteLine("OK in {0}ms", tTimer.ElapsedMilliseconds);
            tTimer = System.Diagnostics.Stopwatch.StartNew();
            RunTests(tParser);
            Console.WriteLine(" ... in {0}ms", tTimer.ElapsedMilliseconds);
         }
         catch (Exception e)
         {
            Console.WriteLine("ERROR: " + e.Message);
            Console.Read();
            return;
         }

         for (; ; )
         {
            Console.Write(">");
            try
            {
               String tLine = Console.ReadLine();
               if (tLine == "!")
                  return;

               tTimer = System.Diagnostics.Stopwatch.StartNew();
               SyntaxNode tResult = tParser.Parse(tLine);
               Console.WriteLine("OK in {0}ms", tTimer.ElapsedMilliseconds);
               Console.WriteLine("[Flattened] Result:");
               new FlatteningVisitor().Flatten((ProductionSyntaxNode)tResult);
               SyntaxToStringVisitor tVisitor = new SyntaxToStringVisitor();
               tResult.Accept(tVisitor);
               Console.WriteLine(tVisitor.ToString());
            }
            catch (Exception e)
            {
               Console.WriteLine("ERROR: " + e.Message);
               //      Console.WriteLine(e.StackTrace);
            }
         }
      }

      // Fucking MSTest is shitting out on these tests with binding failures and other undeterministic behaviour.
      // Do here for now.
      static void RunTests(ParserBase pParser)
      {
         // Simple parse, expecting no exceptions.
         foreach (String tXPath in XPath2Tests.XPathParserTests.ValidXPathExpressions)
         {
            try
            {
               pParser.Parse(tXPath);
            }
            catch (Exception e)
            {
               Console.WriteLine("TEST FAILED: xpath = " + tXPath + " error = " + e.Message);
               return;
            }
         }
         Console.Write("TESTS PASSED");
      }
   }
}
