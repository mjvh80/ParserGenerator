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
         this.Label = "EOF";
         this.GetDecisionTerminals = l => { throw new ParseException("EOF decision terminals needed"); };
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

      //// TODO: check ok
      //// todo: experimental
      //// as we're needing lookahead, we can assume that decision terminals were needed, ie we can call GetDecisionTe..
      //public static ParseNode Lookahead(this ParseNode node, String lookaheadTerminal)
      //{
      //   return new ParseNode()
      //   {
      //      Label = node.Label + "[" + lookaheadTerminal + "]",
      //      GetDecisionTerminals = (level) =>
      //      {
      //         if (level == 0) return node.GetDecisionTerminals(level);
      //         // todo: allow multiple levels?
      //         // todo: must do better lookahead error checking (ie check level int)
      //         return new[] { lookaheadTerminal }; // todo: we should allow more as params or so
      //      },
      //      Parse = node.Parse
      //   };
      //}

      //public static ParseNode Not(this ParseNode node, ParseNode notNode)
      //{
      //   // decision terminals: node, optional is an error
      //   // parse: node.parse, then negation parse on notNode -> extra mode which indicates parsing should fail if flag said
      //   // and parse method succeeds
      //   // how?
      //   return null;
      //}

      public static ParseNode Terminal(this String terminalStr)
      {
         return new ParseNode()
         {
            Label = "TERMINAL(" + terminalStr + ")",
            GetDecisionTerminals = (level) => {
               if (level == 0)
                  return new[] { terminalStr };
               return new String[] { };
            },
            Parse = c => { c.Advance(terminalStr); },
            ParsesTerminal = s => s == terminalStr
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

      private static String OrLabels(ParseNode[] tArray)
      {
         String tResult = "";
         for (Int32 i = 0; i < tArray.Length; i++)
         {
            tResult += tArray[i].Label;
            if (i != tArray.Length - 1)
               tResult += ", ";
         }
         return tResult;
      }

      // todo: check count of params, do like followedby?
      public static ParseNode Or(this ParseNode action, params ParseNode[] otherActions)
      {
         ParseTable<ParseNode> tParseTable = new ParseTable<ParseNode>();
         
         return new ParseNode()
         {
            Primer = (s) =>
               {
                  Boolean tAllPrimed = true;

                  tAllPrimed = action.Prime(s) && tAllPrimed;
                  if (tAllPrimed)
                  foreach (ParseNode tNode in otherActions)
                     tAllPrimed = tNode.Prime(s) && tAllPrimed;

                  if (!tAllPrimed)
                     return false;

                  // With all nodes primed, build parse table.

                  foreach (String tTerminal in action.DoGetDecisionTerminals(0))
                     tParseTable.AddTerminal(tTerminal, action); // todo: better error handling if key found
                  foreach (ParseNode tOtherNode in otherActions)
                     foreach (String tTerminal in tOtherNode.DoGetDecisionTerminals(0)) // todo: must do properly
                        if (tParseTable.ContainsTerminal(tTerminal)) // todo: must do this everywhere...
                        {
                           tParseTable[tTerminal] = new LookaheadParseNode(tTerminal, tParseTable[tTerminal], tOtherNode);
                        }
                        else
                           tParseTable.AddTerminal(tTerminal, tOtherNode);

                  return true; // we are primed
               },

            GetDecisionTerminals = (level) => {
               //if (level == 0)

               if (level != 0) throw new InvalidOperationException("level no longer supported: todo remove"); // todo

               return tParseTable.Terminals.ToArray(); // todo array?
               //return tParseTable.Keys.ToArray();
               //else
               //{
               //   // todo: do more efficient?
               //   String[] tLookaheadTerminals = new String[] { };
               //   tLookaheadTerminals.Merge(action.GetDecisionTerminals(level));
               //   foreach (ParseNode tNode in otherActions)
               //      tLookaheadTerminals.Merge(tNode.GetDecisionTerminals(1));
               //   return tLookaheadTerminals;
               //}
            },
            Parse = c => 
            {
               Int32 tPosition = c.Position;
               c.AdvanceInterleaved(); // todo: must integrate into walker
               var tWalker = tParseTable.GetWalker();
               for (Int32 i = c.Position; i < c.Expression.Length && tWalker.Narrows(c.Expression[i]); i++) ;
               if (tWalker.Value == null)
                  RaiseExpectedTerminals(c, tParseTable.Terminals);
               else
               {
                  c.Position = tPosition;
                  tWalker.Value.Parse(c);
               }
               

               //// Find correct branch, follow it.
               //String tCurrentTerminal = c.Peek();
               //ParseNode tActiveNode;
               //if (tParseTable.TryGetValue(tCurrentTerminal, out tActiveNode))
               //   tActiveNode.Parse(c);
               //else // it may be that we peeked too much, do a binary search:
               //{
               //   // todo: omg, do this better, perhaps not even using a dict
               //   tActiveNode = (from n in tParseTable.Keys where tCurrentTerminal.StartsWith(n) select tParseTable[n]).FirstOrDefault();
               //   if (tActiveNode == null)
               //      RaiseExpectedTerminals(c, tParseTable.Keys);
               //   else
               //      tActiveNode.Parse(c);
               //}
            },
            LookaheadTerminals = (s,l) => tParseTable[s].LookaheadTerminals(s,l), // follow path to where we want lookahead terminals from
            //ParsesTerminal = s => tParseTable.ContainsKey(s) && tParseTable[s].ParsesTerminal(s),
            Label = "OR(" + action.Label + OrLabels(otherActions) + ")",
            Optional = action.Optional || otherActions.Any(n => n.Optional),
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
            // todo: this does the merge, all the time, redo via closure
            //GetDecisionTerminals = node.Optional ? (level) => node.GetDecisionTerminals(level).Merge(otherNode.GetDecisionTerminals(level))
            //                                     : node.GetDecisionTerminals,

            Primer = (s) => {

               //return node.Prime(s) &&
               //otherNode.Prime(s);

               if (node.Optional)
                  return node.Prime(s) && otherNode.Prime(s);
               else
               {
                  Boolean tResult = node.Prime(s);
                  otherNode.Prime(s);
                  return tResult;
               }
            },

            GetDecisionTerminals = level => node.Optional ? node.DoGetDecisionTerminals(0).Merge(otherNode.DoGetDecisionTerminals(0)) : node.DoGetDecisionTerminals(0),

            Parse = c =>
            {
               node.Parse(c);
               otherNode.Parse(c);
            },

            LookaheadTerminals = (s, termList) =>
            {
               if (node.ParsesTerminal(s))
               {
                  termList.AddRange(otherNode.DoGetDecisionTerminals(0));
                  return !otherNode.Optional; // if optional, we may not be done
               }
               else if (node.LookaheadTerminals(s, termList) && !node.Optional)
                  return true; // exhausted
               else
                  return otherNode.LookaheadTerminals(s, termList) && !otherNode.Optional;
            },

            //ParsesTerminal = s => node.ParsesTerminal(s) || (node.Optional && otherNode.ParsesTerminal(s)),

            Label = "FOLLOW(" + node.Label + ", " + otherNode.Label + ")",
            Optional = node.Optional && otherNode.Optional,
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
         ParseTable<ParseNode> tParseTable = null;
        
         return new ParseNode()
         {
            Primer = (s) =>
               {
                  if (node.Prime(s))
                  {
                     if (upperBound - requiredCount > 0)
                     {
                        tParseTable = new ParseTable<ParseNode>();
                        foreach (String tTerminal in node.DoGetDecisionTerminals(0))
                           tParseTable.AddTerminal(tTerminal, node);
                     }
                     return true;
                  }
                  else
                     return false;
               },

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
                  Int32 tPosition = c.Position;
                  c.AdvanceInterleaved();
                  var tWalker = tParseTable.GetWalker();
                  for (Int32 k = c.Position; k < c.Expression.Length && tWalker.Narrows(c.Expression[k]); k++) ;
                  c.Position = tPosition;

                  if (tWalker.Value == null)
                     break; // we're done
                  else
                     tWalker.Value.Parse(c);

                  //String tCurrentTerminal = c.Peek();
                  //if (tParseTable.ContainsKey(tCurrentTerminal))
                  //   node.Parse(c);
                  //else
                  //   break; // we're done
               }
            },
            LookaheadTerminals = (s, termList) =>
            {
               if (node.LookaheadTerminals(s, termList) && requiredCount > 0)
                  return true; // required, should stop
               else
                  return false;
            },
            Label = "OPTIONAL(" + node.Label + ")",
            Optional = requiredCount == 0
         };
      }

      public static ParseNode Optional(this String terminalStr)
      {
         return Optional(terminalStr.Terminal());
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

      public Int32 Position { get; set; } // todo: check usages, external

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

      public virtual void AdvanceInterleaved() { }

      /// <summary>
      /// Used to parse "simple" tokens, language constructs.
      /// Things like strings, numbers etc. should be coded directly. In other words, this should parse
      /// any parts of the BNF contained in quotes.
      /// 
      /// Default implementation parses according to ???
      /// </summary>
      /// <returns></returns>
      protected virtual String AdvanceToken()
      {
         String tResult = "";
         for (; Position < Expression.Length; Position++)
         {
            Char tChar = Expression[Position];
           // if (Char.IsLetter(tChar) || Char.IsDigit(tChar) || Char.IsSymbol(tChar))
            if (!Char.IsWhiteSpace(tChar)) // todo...
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

   //public delegate void PrimeDelegate(Stack<PrimeDelegate> primeStack);

   public class ParseNode
   {
      public String Label; // used for debugging only todo: can use a class type for each type of node (?)

      public Boolean Optional = false;
      public Func<Int32, String[]> GetDecisionTerminals;
      public Action<ParseContext> Parse;

      //public String FindLookaheadTerminal(String terminal) = s => null;
      public Func<String, List<String>, Boolean> LookaheadTerminals = (s, l) => true; // return true if route is exhausted, false otherwise
      public Func<String, Boolean> ParsesTerminal = s => false; // todo: can't we do better?

      public Func<Stack<ParseNode>, Boolean> Primer;

      protected enum PrimeMode { None, Priming, Primed };
      protected PrimeMode Mode = PrimeMode.None;
      public virtual Boolean Prime(Stack<ParseNode> primeStack) 
      {
         switch (Mode)
         {
            case PrimeMode.None:
               if (Primer != null)
               {
                  Mode = PrimeMode.Priming;
                  if (Primer(primeStack))
                     Mode = PrimeMode.Primed;
                  else
                  {
                     Mode = PrimeMode.None;
                     primeStack.Push(this); // couldn't prime this node, queue for retry
                  }
               }
               else // no primer = default primer
                  Mode = PrimeMode.Primed;
               break;
         }

         return Mode == PrimeMode.Primed;
      }

      public String[] DoGetDecisionTerminals(Int32 pLevel) // todo: rename this and lambda
      {
         switch (Mode)
         {
            case PrimeMode.None:
               throw new InvalidOperationException("node not primed");

            case PrimeMode.Priming:
               throw new ParseException("circular grammar");

            default:
               return GetDecisionTerminals(pLevel);
         }
      }

      public Boolean IsPriming() { return Mode == PrimeMode.Priming; }
      public Boolean IsPrimed() { return Mode == PrimeMode.Primed; }
   }

   // only intended for insertion
   public class LookaheadParseNode : ParseNode
   {
      // todo: must make this more general, ability to add nodes if conflicts are more general...
      public LookaheadParseNode(String terminalStr, ParseNode left, ParseNode right)
      {
         Label = "LOOKAHEAD";
         GetDecisionTerminals = (level) =>
            {
               throw new InvalidOperationException(); // todo: return terminalStr? I think it makes no sense to ask for dec. terminals here.
            };

        //Dictionary<String, ParseNode> tParseTable = new Dictionary<string,ParseNode>();
         ParseTable<ParseNode> tParseTable = new ParseTable<ParseNode>();
         List<String> tLookaheadTerminals = new List<String>();
         left.LookaheadTerminals(terminalStr, tLookaheadTerminals);
         foreach (String tTerminal in tLookaheadTerminals)
            tParseTable.AddTerminal(tTerminal, left);
            //tParseTable[tTerminal] = left;
         tLookaheadTerminals.Clear();
         right.LookaheadTerminals(terminalStr, tLookaheadTerminals);
         foreach (String tTerminal in tLookaheadTerminals)
            tParseTable.AddTerminal(tTerminal, right); // todo as below
            //tParseTable[tTerminal] = right; // todo: must catch key already found exception better
        

         Parse = c =>
            {
               Int32 tPosition = c.Position;
               c.Advance(terminalStr);
               c.AdvanceInterleaved(); // todo... dont want this like this -> integrate walker into context
               var tWalker = tParseTable.GetWalker();
               for (Int32 i = c.Position; i < c.Expression.Length && tWalker.Narrows(c.Expression[i]); i++) ;
               if (tWalker.Value == null)
                  throw new Exception("expected oned of " + tParseTable.Terminals.ToArray().ToString());
               //RaiseExpectedTerminals(c, tParseTable.Terminals);
               else
               {
                  c.Position = tPosition; // reset
                  tWalker.Value.Parse(c); // parse
               }


               //// Back up current position.
               //Int32 tPos = c.Position;
               //c.Advance(terminalStr); // expect this terminal
               //ParseNode tChosenNode = tParseTable[c.Peek()]; // todo: same problem as always... 
               //c.Position = tPos;
               //tChosenNode.Parse(c);
            };
      }
   }

   public class SymbolNode : ParseNode
   {
      protected ParseNode mNode;

      public SymbolNode(Func<ParseNode> lazy)
      {
         Label = "SYMBOL"; // todo?

         Primer = (s) =>
         {
            mNode = lazy();
            Boolean tResult = mNode.Prime(s);

            Label = mNode.Label;
            // todo: a symbol node is never optional?
            Optional = mNode.Optional; // todo: make lambda?

            return tResult;

            //this.Primer = mNode.Primer;
            //this.GetDecisionTerminals = mNode.GetDecisionTerminals;
            //this.Parse = mNode.Parse;
            //this.Label = mNode.Label;
            //this.LookaheadTerminals = mNode.LookaheadTerminals;
            //this.Optional = mNode.Optional;
         };

         GetDecisionTerminals = l => mNode.GetDecisionTerminals(l);

         Parse = c => mNode.Parse(c);

         LookaheadTerminals = (s, l) => mNode.LookaheadTerminals(s, l);
      }
   }


   // Idea:
   // A symbolnode is defined by the Rule method.
   // A -> B
   // B -> A 'a'
   // D -> A | B
   // The above would be detected.
   public class SymbolNode_OLD : ParseNode
   {
      protected enum PrimingState { None, Priming, Primed };

      public Func<ParseNode> Primer;
      protected PrimingState State = PrimingState.None;

      protected Func<Int32, String[]> mInternalDecisionSymbols;
      protected Action<ParseContext> mInternalParse;

      public SymbolNode_OLD()
      {
         Label = "__SYMBOL";

         mInternalDecisionSymbols = (level) =>
         {
            if (State == PrimingState.Priming)
               throw new Exception("circular grammar");
            else if (State == PrimingState.None)
            {
               return this.Prime().GetDecisionTerminals(level);
            }

            throw new InvalidOperationException();
         };

         mInternalParse = c => 
         {
            switch (State)
            {
               case PrimingState.None:
                  this.Prime().Parse(c);
                  break;

               // No parsing should be possible during priming.
               case PrimingState.Priming:
                  throw new InvalidOperationException();

               case PrimingState.Primed:
                  this.Parse(c);
                  break;
            }
          //  throw new Exception("no parse method defined"); 
         };

         GetDecisionTerminals = (level) => mInternalDecisionSymbols(level);
         Parse = c => mInternalParse(c);
      }

      public virtual ParseNode Prime()
      {
         if (this.State == PrimingState.Primed)
            return this;

         if (this.State == PrimingState.Priming)
            throw new Exception("attempt to prime priming node");// todo

         this.State = PrimingState.Priming;

         ParseNode tPrimeNode = Primer();
         this.mInternalDecisionSymbols = tPrimeNode.GetDecisionTerminals; // If a symbolnode, it will replace it's internals, but external ref. is constant todo: why did the capture not work?
         this.mInternalParse = tPrimeNode.Parse;
         this.LookaheadTerminals = tPrimeNode.LookaheadTerminals; // todo: need internals?
         //this.GetDecisionTerminals = level => 
         //   tPrimeNode.GetDecisionTerminals(level);
         //this.Parse = c => 
         //   tPrimeNode.Parse(c);
         this.Label = tPrimeNode.Label;

         this.State = PrimingState.Primed;
         return this;
      }
   }

   // todo: this is a proof of concept, must use an efficient algorithm instead.

   /* Datastructure representing the parsetable. It should do:
   * Given the input stream, start a search for characters as they are presented.
   * Once no more match is found, backtrack to longest result.
   * */

   // todo: idea
   // refactor and move walker logic into context (pass a table to it?)
   // - currently a decision is made continuously, causing a a lot of unnecessary retrying..
   // - im not user if we could get that much faster, we still have to make the choice at each point, may be able to cache a prefix
   public class ParseTable<T> where T : class
   {
      protected Dictionary<String, T> Table = new Dictionary<string, T>();

      public void AddTerminal(String terminalStr, T node) 
      {
         if (String.IsNullOrEmpty(terminalStr))
            throw new ArgumentException("key empty or null");

         if (node == null)
            throw new ArgumentException("value is null");

         Table.Add(terminalStr, node);
      }

      public Boolean ContainsTerminal(String terminalStr)
      {
         return Table.ContainsKey(terminalStr);
      }

      public T this[String terminalStr]
      {
         get { return Table[terminalStr]; }
         set 
         {
            if (value == null)
               throw new ArgumentNullException();

            Table[terminalStr] = value; 
         }
      }

      public Boolean TryGetNode(String terminalStr, out T node)
      {
         return Table.TryGetValue(terminalStr, out node);
      }

      public IEnumerable<String> Terminals { get { return Table.Keys; } }

      //// hideously inefficient
      //public Boolean ContainsKeyPrefix(String key)
      //{
      //   if (String.IsNullOrEmpty(key))
      //      return false;

      //   Walker tWalker = new Walker(Table);
      //   Int32 tCount = 0;
      //   for (Int32 i = 0; i < key.Length && tWalker.Narrows(key[i], out tCount); i++) ;

      //   return tCount == 1;
      //}

      public Walker GetWalker()
      {
         return new Walker(Table);
      }

      public class Walker
      {
         public Walker(Dictionary<String, T> pTable) { mTable = pTable; }

         protected String mLastFullKey;
         protected String mKey;
         protected String mBuffer = "";
         protected Dictionary<String, T> mTable;

         public Int32 Count = 0;

         public Boolean Narrows(Char pChar)
         {
            Int32 tCount;
            return Narrows(pChar, out tCount);
         }

         // Returns true if the char narrows the search, but is not yet unambiguous.
         // Returns false if no results or a single result remain.
         // True indicates the search narrows but is not yet finished.
         // todo: inefficient
         public Boolean Narrows(Char pChar, out Int32 tCount)
         {
            mBuffer += pChar;

            tCount = 0;

            foreach (String tKey in mTable.Keys)
               if (tKey.StartsWith(mBuffer))
               {
                  if (tKey.Length == mBuffer.Length)
                     mLastFullKey = tKey;

                  mKey = tKey;
                  tCount += 1;
               }

            if (tCount > 1)
            {
               mKey = null;
               return true; // it narrowed, there's more
            }
               return false; // can stop, key is found (or not, but that's a parse error)
         }

         // assumes things have narrowed...
         public T Value
         {
            get
            {
               if (mKey == null)
               {
                  if (mLastFullKey == null)
                     return null;

                  return mTable[mLastFullKey];
               }
               else
                  return mTable[mKey];
               //foreach (String tKey in mTable.Keys)
               //   if (tKey.StartsWith(mBuffer))
               //      return mTable[tKey];
               //throw new Exception("value not found");
            }
         }
      }
   }

  

   

   //public class ParseTable<T>
   //{
   //   ParseTableNode<T> Root;

   //   public ParseTable() { }

   //   public Boolean ContainsKey(String key)
   //   {
   //      if (String.IsNullOrEmpty(key))
   //         return false;

   //      return Root.ContainsKey(key);
   //   }

   //   public T this[String key]
   //   {
   //      get
   //      {
            
   //      }
   //      set
   //      {
            
   //      }
   //   }
   //}

   //// todo: implement as trie instead?
   //public class ParseTableNode<T>
   //{
   //   public Dictionary<Char, ParseTableNode<T>> Kids;

   //   // Returns true if this contains the key, or only a prefix thereof.
   //   public Boolean ContainsKey(String key)
   //   {
   //      return ContainsKey(key, 0);
   //   }

   //   protected Boolean ContainsKey(String key, Int32 index)
   //   {
   //      ParseTableNode<T> tNode;
   //      return index == key.Length || (Kids.TryGetValue(key[index], out tNode) && tNode.ContainsKey(key, index + 1));
   //   }

   //   public T GetValue(String key)
   //   {
   //      return GetValue(key, 0);
   //   }

   //   protected T GetValue(String key, Int32 index)
   //   {

   //   }

   //   public ParseTableNode<T> Add(String term, T value)
   //   {
   //      if (Kids == null)
   //         Kids = new Dictionary<char, ParseTableNode<T>>();

   //      if (term.Length == 1)
   //         Kids.Add(term[0], new ParseTableValueNode<T>() { Value = value });
   //      else if (term.Length > 0)
   //      {
   //         ParseTableNode<T> tNode;
   //         if (Kids.TryGetValue(term[0], out tNode))
   //            tNode.Add(term.Substring(1), value);
   //         else
   //            Kids.Add(term[0], new ParseTableNode<T>().Add(term.Substring(1), value));
   //      } 

   //      return this;
   //   }
   //}

   //public class ParseTableValueNode<T> : ParseTableNode<T>
   //{
   //   public T Value;
   //}
}
