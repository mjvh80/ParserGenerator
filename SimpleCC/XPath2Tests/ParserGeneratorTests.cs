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
      public void TestLookaheadAmbiguousGrammars()
      {


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
   }
}
