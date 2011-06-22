using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using SimpleCC;
using System.Linq.Expressions;
using XPath2;

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
         ParseNode A = null, B = null; // no production for B
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

   internal class foo
   {
      protected void foobar()
      {
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

   internal class CircularGrammar3 : TestParser
   {
      protected override void DefineGrammar()
      {
         Root = Rule(() => Root);
      }
   }

   internal class CircularGrammar4 : TestParser
   {
      protected override void DefineGrammar()
      {
         ParseNode A = null;
         Root = Rule(() => A);
         A = Rule(() => A.Or(A));
      }
   }

   internal class SimpleMath : TestParser
   {
      protected ParseNode Multiplication, Expr, Factor, Constant, Digit;

      protected override void DefineGrammar()
      {
         Define(out Root, () => Expr);
         Define(out Expr, () => Multiplication.FollowedBy("\\+".FollowedBy(Multiplication).Optional()));
         Define(out Multiplication, () => Factor.FollowedBy("\\*".FollowedBy(Multiplication).Optional()));
         Define(out Factor, () => Constant.Or("\\(".FollowedBy(Expr, "\\)")));
         Define(out Constant, () => Digit.OneOrMore()); //> emit list of digits (1 or more)
         // todo: use regex instead
         Define(out Digit, () => "1".Or("2").Or("3").Or("4").Or("5").Or("6").Or("7").Or("8").Or("9").Or("0"));

         // Flatten the graph to production derivations.
         Root.Rewrite(n => new FlatteningVisitor().Flatten((ProductionSyntaxNode)n));

         // Rewrite constants to return digits, and set compiler.
         Constant.FlattenProduction().Rewrite(n =>
            {
               String tNumber = "";
               foreach (SyntaxNode tValue in n.Children)
                  tNumber = tNumber + (String)tValue.Children[0].Value;

               return new ValueSyntaxNode()
               {
                  Compiler = n2 => Expression.Constant(Int32.Parse((String)n2.Value)),
                  Value = tNumber
               };
            });

         // Compile a factor:
         Factor.SetCompiler(n =>
            {
               if ((String)n.Children[0].Value == "(") // expression
               {
                  return n.Children[1].Compile();
               }
               else
                  return n.Children[0].Compile();
            });

         Multiplication.SetCompiler(n =>
            {
               Expression tLeft = n.Children[0].Compile();

               if (n.Children.Length == 1)
                  return tLeft;

               return Expression.Multiply(tLeft, n.Children[2].Compile());
            });

         Expr.SetCompiler(n =>
            {
               Expression tLeft = n.Children[0].Compile();

               if (n.Children.Length == 1)
                  return tLeft;

               return Expression.Add(tLeft, n.Children[2].Compile());
            });

         Root.SetCompiler(n => n.Children[0].Compile());

      }

   }

   [TestClass]
   public class ParserGeneratorTests
   {
      [TestMethod]
      public void TestMath()
      {
         ParserBase tParser = new SimpleMath().Build();
         SyntaxNode tResult = tParser.Parse("1+1");

         Expression tExpr = tResult.Compile();

         Int32 tValue = Expression.Lambda<Func<Int32>>(tExpr).Compile()();
         Assert.AreEqual(2, tValue);

         
         //new SimplifyingVisitor().Simplify(tResult).Compile();

         //         Expression tExpr = new SimpleMath.Compiler((SimpleMath)tParser).Compile(tResult.Children[0]); // skin root

         //       Int32 tValue = Expression.Lambda<Func<Int32>>(tExpr).Compile()();


         tResult = tParser.Parse("3*1+2");
         tValue = Expression.Lambda<Func<Int32>>(tResult.Compile()).Compile()();
         Assert.AreEqual(5, tValue);

         tResult = tParser.Parse("3*(1+2*2)");
         tValue = Expression.Lambda<Func<Int32>>(tResult.Compile()).Compile()();
         Assert.AreEqual(15, tValue);
      }


      [TestMethod]
      public void TestCircularGrammars()
      {
         TestBadGrammar(new CircularGrammar1(), "grammar is circular, should not build");
         TestBadGrammar(new CircularGrammar2(), "grammar is circular, should not build");
         TestBadGrammar(new CircularGrammar3(), "grammar is circular, root refers to root without other choices");
         TestBadGrammar(new CircularGrammar4(), "grammar is circular, should not build");
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
      public void ReflectionTest()
      {
         String foobar = "";
         Assert.AreEqual("foobar", GetName(() => foobar));
         //Define(() => foobar, Foobar);

         Assert.AreEqual("foobar", Define(() => foobar, "raboof"));
         Assert.AreEqual("raboof", foobar);


         // Foobar = Define("Foobar", () => Foobar.Or(Baz))
      }

      protected String InitFoobar(out String param)
      {
         param = "";
         return "";
      }

      protected String Define<T>(Expression<Func<T>> fn, String definition)
      {
         MemberExpression memExpr = ((MemberExpression)fn.Body);

         Expression assignDefinition = Expression.Assign(memExpr, Expression.Constant(definition));
         
         // Perform assignment.
         ((Func<String>)Expression.Lambda(assignDefinition).Compile())();

         return memExpr.Member.Name;
      }

      protected String GetName(Object anonymous)
      {
         return anonymous.GetType().GetProperties()[0].Name;
      }

      protected String GetName<T>(Expression<Func<T>> fn)
      {
         return ((MemberExpression)fn.Body).Member.Name;
      }
   }
}
