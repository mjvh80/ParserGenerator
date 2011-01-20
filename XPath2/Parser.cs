using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Parser
{
   public class ParseException : Exception
   {
      public ParseException(String msg) : base(msg) { }
      public ParseException(String format, params Object[] args) : base(String.Format(format, args)) { }
   }

   public class EOFNode : ParseNode
   {
      public EOFNode()
      {
         this.GetDecisionTerminals = null; // todo: correct?
         this.Parse = c =>
         {
            if (c.Position != c.Expression.Length)
               throw new ParseException("expected end of input at {0}", c.Position.ToString());
         };
      }
   }

   public static class ParserExtensions
   {
      private static void RaiseExpectedTerminals(ParseContext ctx, IEnumerable<String> terminals)
      {
         // todo: add positional info
         throw new ParseException("expected one of '{0}', found '{1}'", String.Join(", ", terminals), "foobar"); // todo
      }

      public static ParseNode Terminal(this String terminalStr)
      {
         return new ParseNode()
         {
            GetDecisionTerminals = () => new[] { terminalStr },
            Parse = c => { c.Advance(terminalStr); }
         };
      }

      /// <summary>
      /// Same as .FollowedBy(new EOFNode()).
      /// </summary>
      /// <param name="node"></param>
      /// <returns></returns>
      public static ParseNode EOF(this ParseNode node)
      {
         return node.FollowedBy(new EOFNode());
      }

      public static ParseNode Or(this String terminalStr, String otherTerminal)
      {
         return Or(terminalStr.Terminal(), otherTerminal.Terminal());
      }

      public static ParseNode Or(this ParseNode node, String terminalStr)
      {
         return Or(node, terminalStr.Terminal());
      }

      // todo: check count of params, do like followedby?
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
            Optional = action.Optional || otherActions.Any(n => n.Optional),
            GetDecisionTerminals = () => tParseTable.Keys.ToArray(),
            Parse = c => 
            {
               // Find correct branch, follow it.
               String tCurrentTerminal = c.Peek();
               ParseNode tActiveNode;
               if (tParseTable.TryGetValue(tCurrentTerminal, out tActiveNode))
                  tActiveNode.Parse(c);
               else // it may be that we peeked too much, do a binary search:
               {
                  // todo: omg, do this better, perhaps not even using a dict
                  tActiveNode = (from n in tParseTable.Keys where tCurrentTerminal.StartsWith(n) select tParseTable[n]).FirstOrDefault();
                  if (tActiveNode == null)
                     RaiseExpectedTerminals(c, tParseTable.Keys);
                  else
                     tActiveNode.Parse(c);
               }
            }
         };
      }

      private static T[] Merge<T>(this T[] left, T[] right)
      {
         T[] tResult = new T[left.Length + right.Length];
         Array.Copy(left, tResult, left.Length);
         Array.Copy(right, 0, tResult, left.Length, right.Length);
         return tResult;
      }

      public static ParseNode FollowedBy(this ParseNode node, ParseNode otherNode)
      {
         return new ParseNode()
         {
            Optional = node.Optional && otherNode.Optional,

            // todo: this does the merge, all the time, redo via closure
            GetDecisionTerminals = node.Optional ? () => node.GetDecisionTerminals().Merge(otherNode.GetDecisionTerminals())
                                                 : node.GetDecisionTerminals,

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

      // todo: can do this much more efficiently, ie create a parsenode, then iterate others and call parse etc.
      public static ParseNode FollowedBy(this ParseNode node, ParseNode otherNode, params Object[] others)
      {
         ParseNode tFirstNode = node.FollowedBy(otherNode);
         foreach (Object tNode in others)
            if (tNode is String)
               tFirstNode = tFirstNode.FollowedBy(Terminal((String)tNode));
            else
               tFirstNode = tFirstNode.FollowedBy((ParseNode)tNode);
         return tFirstNode;
      }

      public static ParseNode FollowedBy(this String node, String terminalStr, params Object[] others)
      {
         return FollowedBy(Terminal(node), Terminal(terminalStr), others);
      }

      public static ParseNode FollowedBy(this String terminalStr, ParseNode node, params Object[] others)
      {
         return FollowedBy(Terminal(terminalStr), node, others);
      }

      public static ParseNode FollowedBy(this ParseNode node, String terminalStr, params Object[] others)
      {
         return FollowedBy(node, Terminal(terminalStr), others);
      }

      private static ParseNode ParseN(this ParseNode node, Int32 requiredCount, Int32 upperBound)
      {
         // A hashset would do here.
         Dictionary<String, ParseNode> tParseTable = new Dictionary<string, ParseNode>();

         foreach (String tTerminal in node.GetDecisionTerminals())
            tParseTable.Add(tTerminal, node);

         return new ParseNode()
         {
            Optional = requiredCount == 0,
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
         // Advance interleaved characters such as whitespace and/or comments.
         AdvanceInterleaved();

         // Return the next token.
         return (CurrentToken = AdvanceToken());
      }

      /// <summary>
      /// Advance, raising an exception if the found terminal does not match the string found.
      /// Updates CurrentToken.
      /// </summary>
      /// <param name="str"></param>
      /// <returns></returns>
      public String Advance(String str) 
      {
         if (str == null)
            throw new ArgumentNullException("Advance: str is null");

         AdvanceInterleaved();

         Int32 i;
         for (i = 0; i < str.Length && Position < Expression.Length && str[i] == Expression[Position]; i++, Position++) ;

         if (i != str.Length)
            throw new ParseException("expected '{0}' around position {1}", str, Position.ToString()); // todo: improve with actual pos

         return (CurrentToken = str);
      }
      
      /// <summary>
      /// Peeks: returns the next token that would be returned were Advance called.
      /// </summary>
      /// <returns></returns>
      // todo: employ peek caching?
      public String Peek()
      {
         Int32 tOldPos = Position;
         String tResult = Advance(); // todo: dont set currentotken.....
         Position = tOldPos;
         return tResult;
      }

      protected virtual void AdvanceInterleaved() { }

      /// <summary>
      /// Used to parse "simple" tokens, language constructs.
      /// Things like strings, numbers etc. should be coded directly. In other words, this should parse
      /// any parts of the BNF contained in quotes.
      /// 
      /// Default implementation parses according to Char.IsLetter || Char.IsDigit || Char.IsSymbol.
      /// </summary>
      /// <returns></returns>
      protected virtual String AdvanceToken()
      {
         String tResult = "";
         for (; Position < Expression.Length; Position++)
         {
            Char tChar = Expression[Position];
            if (Char.IsLetter(tChar) || Char.IsDigit(tChar) || Char.IsSymbol(tChar))
               tResult += tChar;
            else
               break;
         }
         return tResult;
      }
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
      public Boolean Optional = false;
      public Func<String[]> GetDecisionTerminals;
      public Action<ParseContext> Parse;
   }

   // Idea:
   // A symbolnode is defined by the Rule method.
   // A -> B
   // B -> A 'a'
   // D -> A | B
   // The above would be detected.
   public class SymbolNode : ParseNode
   {
      protected enum PrimingState { None, Priming, Primed };

      public Func<ParseNode> Primer;
      protected PrimingState State = PrimingState.None;

      public SymbolNode()
      {
         GetDecisionTerminals = () => 
         {
            if (State == PrimingState.Priming)
               throw new Exception("circular grammar");
            else if (State == PrimingState.None)
            {
               return this.Prime().GetDecisionTerminals();
            }

            throw new InvalidOperationException();
         };
         Parse = c => { throw new Exception("no parse method defined"); };
      }

      public virtual ParseNode Prime()
      {
         if (this.State == PrimingState.Primed)
            return this;

         if (this.State == PrimingState.Priming)
            throw new Exception("attempt to prime priming node");// todo

         this.State = PrimingState.Priming;

         ParseNode tPrimeNode = Primer();
         this.GetDecisionTerminals = tPrimeNode.GetDecisionTerminals;
         this.Parse = tPrimeNode.Parse;

         this.State = PrimingState.Primed;
         return tPrimeNode;
      }
   }
}
