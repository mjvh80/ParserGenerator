using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace XPath2
{
   public class ParseContextSnapshot 
   {
      public Int32 Position;
   }

   public class ParseContext 
   {
      public ParseContextSnapshot Snapshot() 
      {
         return new ParseContextSnapshot { Position = mCurrentPosition };
      }

      // Indicates if current context is required, or optional.
      // may need a stack for this?
      public Boolean Required;

      public ParseContext Restore(ParseContextSnapshot snapshot) 
      {
         mCurrentPosition = snapshot.Position;
         return this;
      }

      protected Int32 mCurrentPosition;
      protected String mExpression;

      public String Expression
      {
         get { return mExpression; }
         set { mExpression = value; }
      }

      public Parser Parser;

      // Advance ignoring whitespace and comments.
      public void AdvanceIgnorable() 
      {
         AdvanceWhitespace();
         // todo: comments
         //if (mExpression[mCurrentPosition] == '(' && mExpression[mCurrentPosition + 1] == ':') // todo: array bounds
         //   AdvanceComment();
      }

      public void AdvanceWhitespace()
      {
         while (mCurrentPosition < mExpression.Length && IsWhitespace(mExpression[mCurrentPosition]))
            mCurrentPosition++;
      }

      protected Boolean IsWhitespace(Char pChar)
      {
         // todo: must follow proper rules
         return Char.IsWhiteSpace(pChar);
      }

      // Advance parser, ensuring the expected string is parsed.
      public Boolean Advance(String expected) 
      {
         Int32 j;
         for (j = 0; j < expected.Length && mCurrentPosition < mExpression.Length && expected[j] == mExpression[mCurrentPosition]; mCurrentPosition++, j++)
            ;
         return j == expected.Length;
      }

      public Boolean AdvanceQName()
      {
         // todo: must do properly...
         String tQName = null;
         for (; mCurrentPosition < mExpression.Length; mCurrentPosition++)
         {
            Char tChar = mExpression[mCurrentPosition];
            if (Char.IsLetter(tChar))
               tQName += tChar;
            else
               break;
         }
         return tQName != null; // will want to return in the future...
      }
   }

   public static class ParserExtensions
   {
      public static ParseAction Optional(this ParseAction action)
      {
         return c =>
            {
               ParseContextSnapshot tSnapshot = c.Snapshot();

               Boolean tOldRequired = c.Required;
               c.Required = false;

               if (!action(c))
                  // we're optional, so must restore as we always return true.
                  c.Restore(tSnapshot);

               c.Required = tOldRequired;
               return true;
            };
      }

      public static ParseAction ZeroOrMore(this ParseAction action)
      {
         return c =>
         {
            ParseContextSnapshot tSnapshot;
            Boolean tOldRequired = c.Required;
            c.Required = false;
            do
            {
               tSnapshot = c.Snapshot();
            } while (action(c));
            c.Restore(tSnapshot);
            c.Required = tOldRequired;
            return true;
         };
      }

      public static ParseAction OneOrMore(this ParseAction action)
      {
         return c =>
         {
            if (action(c))
            {
               return ZeroOrMore(action)(c);
            }
            else // if required, the action will have thrown
               return false;
         };
      }

      public static ParseAction Terminal(this String terminalStr)
      {
         if (terminalStr == null)
            throw new ArgumentNullException("terminal string is null");

         return c =>
         {
            c.AdvanceIgnorable();
            if (c.Advance(terminalStr))
               return true;
            else
            {
               if (c.Required)
                  throw new Exception("expected " + terminalStr); // todo improve
               return false;
            }
            //c.AdvanceIgnorable();
         };
      }

      /// <summary>
      /// Implements A B.
      /// </summary>
      public static ParseAction FollowedBy(this ParseAction action, ParseAction nextAction)
      {
         if (action == null)
            throw new ArgumentNullException("action is null");

         return c => action(c) && nextAction(c);
      }

      public static ParseAction FollowedBy(this String terminalStr, ParseAction action)
      {
         return FollowedBy(Terminal(terminalStr), action);
      }

      public static ParseAction FollowedBy(this ParseAction action, String terminalStr)
      {
         return FollowedBy(action, Terminal(terminalStr));
      }

      public static ParseAction FollowedBy(this String leftTerminal, String rightTerminal)
      {
         return FollowedBy(Terminal(leftTerminal), Terminal(rightTerminal));
      }

      /// <summary>
      /// Implements A | B.
      /// </summary>
      public static ParseAction Or(this ParseAction action, ParseAction nextAction) //, String errorMsg)
      {
         return c =>
            {
               ParseContextSnapshot tSnapshot = c.Snapshot();
               Boolean tOldRequired = c.Required;
               c.Required = false;

               Boolean tSuccess = action(c) || nextAction(c.Restore(tSnapshot));
                 
               //if (!tSuccess && c.Required)
               //   throw new Exception(errorMsg); // improve

               c.Required = tOldRequired;
               return tSuccess;
            };
      }

      // todo: use instead of above
      private static Boolean RunOptional(ParseContext c, ParseAction a)
      {
         Boolean tOldRequired = c.Required;
         Boolean tResult = a(c);
         c.Required = tOldRequired;
         return tResult;
      }

      public static ParseAction Or(this String leftTerm, String rightTerm)
      {
        // return Or(Terminal(leftTerm), Terminal(rightTerm)); //, String.Format("expected '{0}' or '{1}'", leftTerm, rightTerm));

         return Or(Terminal(leftTerm), Terminal(rightTerm)).Error(String.Format("expected '{0}' or '{1}'", leftTerm, rightTerm));
      }

      public static ParseAction Error(this ParseAction action, String msg)
      {
         return c =>
         {
            Boolean tResult = action(c);

            if (!tResult && c.Required)
               throw new Exception(msg); // todo: improve

            return tResult;
         };
      }
   }

   
   public delegate Boolean ParseAction(ParseContext ctx);

   public class Parser
   {
      public Boolean Parse(String xpath2Expression)
      {
         return XPath(new ParseContext()
         {
            Parser = this, Expression = xpath2Expression, Required = true
         });
      }

      ParseAction XPath;

      public Parser()
      {

         //Func<Func<ParseAction>, ParseAction> ForwardDeclare = a => a();

         // Allows for forward declarations of symbols.
         Func<Func<ParseAction>, ParseAction> Rule = f => c => f()(c);

         // Define grammar symbols:
         ParseAction tExpr = null,
            tExprSingle = null,
            tForExpr = null,
            tQuantifiedExpr = null,
            tIfExpr = null,
            tOrExpr = null,
            tSimpleForClause = null,
            tVarName = null,
            tAndExpr = null,
            tComparisonExpr = null,
            tEOF = null;

         XPath = Rule(() => tExpr.FollowedBy(tEOF));

         tExpr = Rule(() => tExprSingle.FollowedBy((",".FollowedBy(tExprSingle)).Optional()));

         tExprSingle = Rule(() => tForExpr.Or(tQuantifiedExpr).Or(tIfExpr).Or(tOrExpr).Error("expected 'if' or 'or'..."));

         tForExpr = Rule(() => tSimpleForClause.FollowedBy("return".FollowedBy(tExprSingle)).Error("expected for clause"));

         tSimpleForClause = Rule(() => "for".FollowedBy("$").FollowedBy(tVarName).FollowedBy("in").FollowedBy(tExprSingle).FollowedBy(
            (",".FollowedBy("$").FollowedBy(tVarName).FollowedBy("in").FollowedBy(tExprSingle)).Optional()).Error("expected 'for'"));

         tQuantifiedExpr = Rule(() => "some".Or("every").FollowedBy("$").FollowedBy(tVarName).FollowedBy("in").FollowedBy(tExprSingle)
            .FollowedBy((",".FollowedBy("$").FollowedBy(tVarName).FollowedBy("in").FollowedBy(tExprSingle)).Optional())
            .FollowedBy("satisfies").FollowedBy(tExprSingle));

         // todo: comment check (:, could do that in terminal symbol.
         tIfExpr = Rule(() => "if".FollowedBy("(").FollowedBy(tExpr).FollowedBy(")").FollowedBy("then").FollowedBy(tExprSingle).FollowedBy("else").FollowedBy(tExprSingle).Error("expected if clause"));

         tOrExpr = Rule(() => tAndExpr.FollowedBy(("or".FollowedBy(tAndExpr)).Optional()).Error("expected or clause"));

         tAndExpr = Rule(() => tComparisonExpr.FollowedBy(("and".FollowedBy(tComparisonExpr)).Optional()).Error("expected and clause"));

         tComparisonExpr = Rule(() => "foo".Terminal());


         // todo: more work
         tVarName = c => c.AdvanceQName();
         tEOF = c => true; // todo: parse whitespace / comments, then test for end of input

         //tSimpleForClause = "for".FollowedBy("$".FollowedBy(tVarName.FollowedBy("in".FollowedBy(tExprSingle.FollowedBy(
         //   (",".FollowedBy("$".FollowedBy(tVarName.FollowedBy("in".FollowedBy(tExprSingle))))).Optional().FollowedBy("satisfies"))))));
      }

      public void ParseWhitespace(ParseContext ctx)
      {
         // to implement: ignore whitespace
      }

      public void ParseComment(ParseContext ctx)
      {
         // to implement: ignore a comment
      }
   }
}
