using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Parser;

namespace XPath2Tests
{
   internal abstract class TestParser : ParserBase
   {
      protected override ParseContext GetContext()
      {
         return new TestParseContext();
      }
  
      protected class TestParseContext : ParseContext { }
   }

   internal class CircularGrammar1 : TestParser
   {
      protected override void  DefineGrammar()
      {
         ParseNode A = null, B = null;

         Root = Rule(() => A);

         A = Rule(() => B);
         B = Rule(() => A);
      }
   }

   internal class LeftRecursiveGrammar1 : TestParser
   {
      protected override void DefineGrammar()
      {
         ParseNode A = null, B = null;

         // A -> A B
         Root = Rule(() => A);
         A = Rule(() => A.FollowedBy(B));
         B = "B".Terminal();
      }
   }

   internal class LL1ProblemGrammar1 : TestParser
   {
      protected override void DefineGrammar()
      {
         // lookahead of more than 1 would be needed here
         Root = Rule(() => "a".FollowedBy("b", "c").Or("a".FollowedBy("b", "d")));
         // A = a b c | a b d
      }
   }

   internal class LL1ProblemGrammar2 : TestParser
   {
      // tests derivability to terminals, problematic grammar from coco docs
      // todo: i think ill need to introduce a check for this into the code
      // each parsenode checks its derivability. This has to be a second run I think.
      protected override void DefineGrammar()
      {
         ParseNode X = null, Y = null;
         Root = Rule(() => X.EOF());
         X = Rule(() => Y.FollowedBy(";"));
         Y = Rule(() => "\\(".FollowedBy(X, "\\)"));
         //X = Y ';'. 
         //Y = '(' X ')'
      }
   }

   internal class LL1ProblemGrammar3 : TestParser
   {
      protected override void DefineGrammar()
      {
         ParseNode A = null, B = null;
         Root = Rule(() => A.Or(B).EOF());
         A = "a".FollowedBy("b").FollowedBy("c");
         B = "a".FollowedBy("b").FollowedBy("d");
      }
   }

   internal class TrickyGrammar1 : TestParser
   {
      protected override void DefineGrammar()
      {
         ParseNode A = null, B = null, C = null;
  
         // Coco/R produces warnings on this
         Root = Rule(() => A.EOF());
         A = Rule(() => "a".Or(B.FollowedBy(C, "d"))); //  A = (a | B C d). 
         B = Rule(() => "b".Optional().FollowedBy("a")); // B = [b] a. 
         C = Rule(() => "c".FollowedBy("d".ZeroOrMore())); //C = c {d}.
      }
   }

   internal class IncompleteGrammar1 : TestParser
   {
      protected override void DefineGrammar()
      {
         ParseNode A = null; // no production for A
         Root = Rule(() => A);
      }
   }

   internal class IncompleteGrammar2 : TestParser
   {
      protected override void DefineGrammar()
      {
         ParseNode A = null, B = null; // no production for A
         Root = Rule(() => A.FollowedBy(B));
         A = "a".Terminal();
      }
   }

   internal class UnreachableTerminal1 : TestParser
   {
      protected override void DefineGrammar()
      {
         ParseNode A = null, B = null;
         Root = Rule(() => A);
         A = "a".Terminal();
         B = Rule(() => "b".Terminal()); // b is not reachable..

         // todo: if we're not using Rule, unreachable terminals are not detected...
      }
   }

   internal class CircularGrammar2 : TestParser
   {
      protected override void DefineGrammar()
      {
         ParseNode A = null, B = null, C = null, D = null;

         Root = Rule(() => A);

         A = Rule(() => B);
         B = Rule(() => C);
         C = Rule(() => D);
         D = Rule(() => B);
      }
   }

   internal class SimpleMath : TestParser
   {
      protected override void DefineGrammar()
      {
         ParseNode Multiplication = null, Expression = null, Factor = null, Constant = null, Digit = null;

         Root = Rule(() => Expression);
         Expression = Rule(() => Multiplication.FollowedBy("\\+".FollowedBy(Multiplication).Optional()));
         Multiplication = Rule(() => Factor.FollowedBy("\\*".FollowedBy(Multiplication).Optional()));
         Factor = Rule(() => Constant.Or("\\(".FollowedBy(Expression, "\\)")));
         Constant = Rule(() => Digit.OneOrMore()); //> emit list of digits (1 or more)
         Digit = Rule(() => "1".Or("2").Or("3").Or("4").Or("5").Or("6").Or("7").Or("8").Or("9").Or("0")); // todo: use regex instead
      }
   }

   //internal class TmpGrammar : TestParser
   //{
   //   protected override void DefineGrammar()
   //   {
   //      ParseNode A = null, B = null, C = null, D = null;

   //      Root = Rule(() => A);
   //      A = Rule(() => B.Emit().FollowedBy(C).Or(D.Emit())).Emit(n => new );
   //   }
   //}

   [TestClass]
   public class ParserGeneratorTests
   {
      [TestMethod]
      public void TestCircularGrammars()
      {
         TestBadGrammar(new CircularGrammar1(), "grammar is circular, should not build");
         TestBadGrammar(new CircularGrammar2(), "grammar is circular, should not build");
      }

      [TestMethod]
      public void TrickyGrammarTest()
      {
         new TrickyGrammar1().Build();
      }

      [TestMethod]
      public void TestLookaheadAmbiguousGrammars()
      {
         TestBadGrammar(new LL1ProblemGrammar1(), "grammar should fail on too much lookahead needed");
       
         // the problem here is this would never parse anything, only an infinite stream of (.
         TestBadGrammar(new LL1ProblemGrammar2(), "derivability issue: infinite parser");

         TestBadGrammar(new LL1ProblemGrammar3(), "should fail due to more than 1 lookahead needed.");
      }

      [TestMethod]
      public void TestLeftRecursion()
      {
         TestBadGrammar(new LeftRecursiveGrammar1(), "left recursion did not fail");
      }

      [TestMethod]
      public void TestMiscellaneous()
      {
         TestBadGrammar(new IncompleteGrammar1(), "incomplete grammar should not parse");
         TestBadGrammar(new IncompleteGrammar2(), "incomplete grammar should not parse");
         TestBadGrammar(new UnreachableTerminal1(), "unreachable terminal not detected");
      }

      [TestMethod]
      public void TestDoubleBuild()
      {
         ParserBase tMath = new SimpleMath().Build();
         try
         {
            tMath.Build();
            Assert.Fail("should not be able to build twice");
         }
         catch (ParseException)
         {
            // ok
         }
      }

      protected void TestBadGrammar(ParserBase pParser, String msg)
      {
         try
         {
            pParser.Build();
            Assert.Fail(msg);
         }
         catch (ParseException)
         {
            // OK
         }

      }

      [TestMethod]
      public void TestMath()
      {
         ParserBase tParser = new SimpleMath().Build();
         tParser.Parse("1+1");
         tParser.Parse("3*1+2");
         tParser.Parse("3*(1+2*2)");
      }
   }
}
