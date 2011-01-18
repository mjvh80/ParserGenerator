using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace XPath2_Second
{
   public class ParseException : Exception
   {
      public ParseException(String msg) : base(msg) { }
      public ParseException(String format, params Object[] args) : base(String.Format(format, args)) { }
   }

   public static class Parser2Extensions
   {
      private static void RaiseExpectedTerminals(ParseContext ctx, IEnumerable<String> terminals)
      {
         // todo: add positional info
         throw new ParseException("expected one of '{0}', found '{1}", String.Join(", ", terminals));
      }

      public static ParseNode Terminal(this String terminalStr)
      {
         return new ParseNode()
         {
            GetDecisionTerminals = () => new[] { terminalStr },
            Parse = c => { c.Advance(terminalStr); }
         };
      }

      public static ParseNode Or(this ParseNode action, params ParseNode[] otherActions)
      {
         Dictionary<String, ParseNode> tParseTable = new Dictionary<String, ParseNode>();
         foreach (String tTerminal in action.GetDecisionTerminals())
            tParseTable.Add(tTerminal, action); // todo: better error handling if key found
         foreach (ParseNode tOtherNode in otherActions)
            foreach (String tTerminal in tOtherNode.GetDecisionTerminals())
               tParseTable.Add(tTerminal, tOtherNode);

         return new ParseNode()
         {
            GetDecisionTerminals = () => tParseTable.Keys.ToArray(),
            Parse = c => 
            {
               // Find correct branch, follow it.
               String tCurrentTerminal = c.Peek();
               ParseNode tActiveNode;
               if (tParseTable.TryGetValue(tCurrentTerminal, out tActiveNode))
                  tActiveNode.Parse(c);
               else
                  RaiseExpectedTerminals(c, tParseTable.Keys);
            }
         };
      }

      public static ParseNode FollowedBy(this ParseNode node, ParseNode otherNode)
      {
         return new ParseNode()
         {
            GetDecisionTerminals = node.GetDecisionTerminals,

            Parse = c =>
            {
               node.Parse(c);
               otherNode.Parse(c);
            }
         };
      }

      public static ParseNode FollowedBy(this String terminalStr, ParseNode node)
      {
         return terminalStr.Terminal().FollowedBy(node);
      }

      private static ParseNode ParseN(this ParseNode node, Int32 requiredCount, Int32 upperBound)
      {
         // A hashset would do here.
         Dictionary<String, ParseNode> tParseTable = new Dictionary<string, ParseNode>();

         foreach (String tTerminal in node.GetDecisionTerminals())
            tParseTable.Add(tTerminal, node);

         return new ParseNode()
         {
            GetDecisionTerminals = node.GetDecisionTerminals,
            Parse = c =>
            {
               // Parse minimal count.
               for (Int32 i = 0; i < requiredCount; i++)
                  node.Parse(c); // simply go in

               // Parse optional max count.
               Int32 tCountLeft = (upperBound == Int32.MaxValue ? Int32.MaxValue : (upperBound - requiredCount));
               for (Int32 i = 0; i < tCountLeft && tCountLeft > 0; i++)
               {
                  String tCurrentTerminal = c.Peek();
                  if (tParseTable.ContainsKey(tCurrentTerminal))
                     node.Parse(c);
                  else
                     break; // we're done
               }
            }
         };
      }

      public static ParseNode Optional(this ParseNode node)
      {
         return ParseN(node, 0, 1);
      }

      public static ParseNode ZeroOrMore(this ParseNode node)
      {
         return ParseN(node, 0, Int32.MaxValue);
      }

      public static ParseNode OneOrMore(this ParseNode node)
      {
         return ParseN(node, 1, Int32.MaxValue);
      }
   }

   public class ParseContext
   {
      public ParseContext()
      {
         Position = 0;
      }

      public String Expression;
      public String CurrentToken;

      public Int32 Position { get; protected set; }

      /// <summary>
      /// Advances to the next token, ignoring interleaved whitespace.
      /// Updates the CurrentToken.
      /// </summary>
      /// <returns></returns>
      public String Advance() 
      {
         // todo: ignore comments etc. etc.
         AdvanceWhitespace();

         // Very simple:
         String tToken = "";
         for (; Position < Expression.Length; Position++)
         {
            Char tChar = Expression[Position];
            if (Char.IsWhiteSpace(tChar))
               break;
            tToken += tChar;
         }

         return tToken;
      }

      public String Advance(String str) 
      {
         String tResult = Advance();
         if (tResult != str) // todo: better equals
            throw new Exception("expected " + str + " found " + tResult);
         return tResult;
      } // advance, throwing if we dont get the provided string
      
      public String Peek()
      {
         // todo: do properly, of course
         Int32 tOldPos = Position;
         String tResult = Advance();
         Position = tOldPos;
         return tResult;
      }

      // todo: create an abstract method instead: parseInterleaved which ditches comments and whitespace etc.
      protected virtual void AdvanceWhitespace()
      {
         while (Position < Expression.Length && Char.IsWhiteSpace(Expression[Position]))
            Position++;
      }

      protected abstract void AdvanceInterleaved();
      protected abstract String AdvanceToken();
   }

   //public class ParseNode_Or : ParseNode
   //{

   //}

   //public class ParseNode
   //{
   //   public virtual String[] GetDecisionTerminals() { throw new NotImplementedException("todo"); }
   //   public void Parse(ParseContext ctx)
   //   {

   //   }
   //}

   public class ParseNode
   {
      public Func<String[]> GetDecisionTerminals;
      public Action<ParseContext> Parse;
   }

   public class XPathParseContext : ParseContext
   {

      protected override void AdvanceInterleaved()
      {
         throw new NotImplementedException();
      }

      protected override string AdvanceToken()
      {
         throw new NotImplementedException();
      }
   }


   public class Parser2
   {
      ParseNode Root;

      public Parser2()
      {
         Func<Func<ParseNode>, ParseNode> Rule = f =>
            new ParseNode()
            {
               GetDecisionTerminals = () => f().GetDecisionTerminals(),
               Parse = c => f().Parse(c)
            };

         ParseNode Expression = null, ExpressionSimple = null, ExprFoo = null, ExprBar = null, ExprBaz = null, ExprZab = null;

         Expression = Rule(() => ExpressionSimple.FollowedBy(",".FollowedBy(ExpressionSimple).Optional()));
         ExpressionSimple = Rule(() => ExprFoo.Or(ExprBar));
         ExprFoo = Rule(() => "foo".Terminal());
         ExprBar = Rule(() => ExprBaz.Or(ExprZab));
         ExprBaz = Rule(() => "baz".Terminal());
         ExprZab = Rule(() => "zab".Terminal());

         Root = Expression;
      }

      public void Parse(String expr)
      {
         Root.Parse(new ParseContext()
         {
            Expression = expr
         });
      }
   }
}
