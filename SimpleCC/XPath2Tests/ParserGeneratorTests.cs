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
         try
         {
            new CircularGrammar1().Build();
            new CircularGrammar2().Build();
            Assert.Fail("should not get here: grammar is circular");
         }
         catch (ParseException e)
         {
            // OK
         }
      }

      [TestMethod]
      public void TrickyGrammarTest()
      {
         new TrickyGrammar1().Build();
      }

      [TestMethod]
      public void TestLookaheadAmbiguousGrammars()
      {
         try
         {
            new LL1ProblemGrammar1().Build();
            Assert.Fail("grammar should fail on too much lookahead needed");
         }
         catch (ParseException e)
         {
            // OK
         }
      }

      [TestMethod]
      public void TestLeftRecursion()
      {
         try
         {
            new LeftRecursiveGrammar1().Build();
            Assert.Fail("left recursion did not fail");
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
