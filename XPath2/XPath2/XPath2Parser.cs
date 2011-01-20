using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Parser;

namespace XPath2.Parser
{
   public class XPathParseContext : ParseContext
   {
      // todo: put this in base
      protected Boolean IsWhitespace(Char pChar)
      {
         // todo: properly
         return Char.IsWhiteSpace(pChar);
      }

      protected Boolean IsChar(Char pChar)
      {
         return true; // todo: xml proper
      }

      // Parse whitespace, return true if space was found.
      protected Boolean AdvanceWhitespace()
      {
         Int32 tOldPos = Position;
         while (Position < Expression.Length && IsWhitespace(Expression[Position]))
            Position++;
         return Position != tOldPos;
      }

      // Parse comments with any nesting level, return true if one was found.
      protected Boolean AdvanceComment()
      {
         Int32 tNestingLevel = 1;

         if (Position + 1 < Expression.Length)
         {
            if (Expression[Position] == '(' && Expression[Position + 1] == ':')
            {
               // Ignore Chars
               for (Position = Position + 2; Position < Expression.Length; Position++)
               {
                  // Start of nested comment.
                  Char tChar = Expression[Position];
                  if (tChar == '(' && Position + 1 < Expression.Length && Expression[Position + 1] == ':')
                  {
                     tNestingLevel += 1;
                     Position += 1;
                     continue;
                  }

                  // End of comment.
                  if (tChar == ':' && Position + 1 < Expression.Length && Expression[Position + 1] == ')')
                  {
                     tNestingLevel -= 1;
                     Position += 1;
                     if (tNestingLevel == 0)
                     {
                        Position += 1;
                        return true;
                     }
                     else
                        continue;
                  }

                  if (!IsChar(tChar))
                     throw new ParseException("invalid character found in comment: '{0}' at position {1}", tChar.ToString(), Position.ToString());
               }

               throw new ParseException("unexpected end of input while reading a comment"); // todo: improve with starting info?
            }
         }

         return false;
      }

      protected override void AdvanceInterleaved()
      {
         while (AdvanceWhitespace() || AdvanceComment()) ; // todo: can probably do little more efficient...
      }
   }

   // todo: move common stuff into a base class
   public class XPath2Parser
   {
      ParseNode Root;

      public XPath2Parser()
      {
         List<SymbolNode> tPrimeTargets = new List<SymbolNode>();

         //Func<Func<ParseNode>, ParseNode> Rule = f =>
         //   new ParseNode()
         //   {
         //      GetDecisionTerminals = () => f().GetDecisionTerminals(),
         //      Parse = c => f().Parse(c)
         //   };

         Func<Func<ParseNode>, ParseNode> Rule = f =>
            {
               SymbolNode tNode = new SymbolNode()
               {
                  Primer = f
               };
               tPrimeTargets.Add(tNode);
               return tNode;
            };

         // Define grammar symbols:
         ParseNode tExpr = null,
            tExprSingle = null,
            tForExpr = null,
            tQuantifiedExpr = null,
            tIfExpr = null,
            tOrExpr = null,
            tSimpleForClause = null,
            tVarName = null,
            tAndExpr = null,
            tComparisonExpr = null,
            tRangeExpr = null, tValueComp = null, tGeneralComp = null, tNodeComp = null,
            tAdditiveExpr = null,
            tMultiplicativeExpr = null,
            tUnionExpr = null,
            tIntersectExceptExpr = null,
            tInstanceofExpr = null,
            tTreatExpr = null,
            tCastableExpr = null,
            tCastExpr = null,
            tUnaryExpr = null,
            tValueExpr = null,
            tSequenceType = null,
            tSingleType = null,
            tPathExpr = null;

         Root = Rule(() => tExpr.EOF());

         tExpr = Rule(() => tExprSingle.FollowedBy(",".FollowedBy(tExprSingle).ZeroOrMore()));

         //tExprSingle = Rule(() => tForExpr.Or(tQuantifiedExpr).Or(tIfExpr).Or(tOrExpr).Error("expected 'if' or 'or'..."));
         tExprSingle = Rule(() => tForExpr.Or(tQuantifiedExpr, tIfExpr, tOrExpr));

         //tForExpr = Rule(() => tSimpleForClause.FollowedBy("return".FollowedBy(tExprSingle)));
         tForExpr = Rule(() => tSimpleForClause.FollowedBy("return", tExprSingle));

         tSimpleForClause = Rule(() => "for".FollowedBy("$", tVarName, "in", tExprSingle, (
               ",".FollowedBy("$", tVarName, "in", tExprSingle).ZeroOrMore()
            )));

         tQuantifiedExpr = Rule(() => ("some".Or("every")).FollowedBy("$", tVarName, "in", tExprSingle,
                ",".FollowedBy("$", tVarName, "in", tExprSingle).ZeroOrMore(),
                "satisfies", tExprSingle)
             );

         // todo: comment check (:, could do that in terminal symbol.
         tIfExpr = Rule(() => "if".FollowedBy("(", tExpr, ")", "then", tExprSingle, "else", tExprSingle));

         tOrExpr = Rule(() => tAndExpr.FollowedBy("or".FollowedBy(tAndExpr).ZeroOrMore()));

         tAndExpr = Rule(() => tComparisonExpr.FollowedBy("and".FollowedBy(tComparisonExpr).ZeroOrMore()));

         tComparisonExpr = Rule(() => tRangeExpr.FollowedBy(tValueComp.Or(tGeneralComp, tNodeComp).FollowedBy(tRangeExpr).Optional()));

         tRangeExpr = Rule(() => tAdditiveExpr.FollowedBy("to".FollowedBy(tAdditiveExpr).Optional()));
         tAdditiveExpr = Rule(() => tMultiplicativeExpr.FollowedBy("-".Or("+").FollowedBy(tMultiplicativeExpr).ZeroOrMore()));
         tMultiplicativeExpr = Rule(() => tUnionExpr.FollowedBy("*".Or("div").Or("idiv").Or("mod").FollowedBy(tUnionExpr).ZeroOrMore()));
         tUnionExpr = Rule(() => tIntersectExceptExpr.FollowedBy("union".Or("|").FollowedBy(tIntersectExceptExpr).ZeroOrMore()));
         tIntersectExceptExpr = Rule(() => tInstanceofExpr.FollowedBy("intersect".Or("except").FollowedBy(tInstanceofExpr).ZeroOrMore()));
         tInstanceofExpr = Rule(() => tTreatExpr.FollowedBy("instance".FollowedBy("of", tSequenceType).Optional())); ;
         tTreatExpr = Rule(() => tCastableExpr.FollowedBy("treat".FollowedBy("as", tSequenceType).Optional()));
         tCastableExpr = Rule(() => tCastExpr.FollowedBy("castable".FollowedBy("as", tSingleType).Optional()));
         tCastExpr = Rule(() => tUnaryExpr.FollowedBy("cast".FollowedBy("as", tSingleType).Optional()));
         tUnaryExpr = Rule(() => "+".Or("-").ZeroOrMore().FollowedBy(tValueExpr));
         tValueExpr = Rule(() => tPathExpr);

         tGeneralComp = "=".Or("!=").Or("<").Or("<=").Or(">").Or(">="); // todo: params overload
         tValueComp = "eq".Or("ne").Or("lt").Or("le").Or("gt").Or("ge");
         tNodeComp = "is".Or("<<").Or(">>");

         // todo: more work
         tVarName = new ParseNode() {
            GetDecisionTerminals = () => { throw new NotSupportedException(""); },
            Parse = c => { c.Advance(); } // todo: this is incorrec,t but for testing...
         };
         // todo
         tSequenceType = "foo".Terminal();
         tSingleType = "foo".Terminal();
         tPathExpr = "foo".Terminal();

         //ParseNode Expression = null, ExpressionSimple = null, ExprFoo = null, ExprBar = null, ExprBaz = null, ExprZab = null;

         //Expression = Rule(() => ExpressionSimple.FollowedBy(",".FollowedBy(ExpressionSimple).Optional()));
         //ExpressionSimple = Rule(() => ExprFoo.Or(ExprBar));
         //ExprFoo = Rule(() => "foo".Terminal());
         //ExprBar = Rule(() => ExprBaz.Or(ExprZab));
         //ExprBaz = Rule(() => "baz".Terminal());
         //ExprZab = Rule(() => "zab".Terminal());


         // Prime.
         foreach (SymbolNode tSymbol in tPrimeTargets)
            tSymbol.Prime();
       //  ((SymbolNode)Root).Prime();
      }

      public void Parse(String expr)
      {
         Root.Parse(new XPathParseContext()
         {
            Expression = expr
         });
      }
   }
}
