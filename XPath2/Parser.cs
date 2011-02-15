using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using SimpleRegexIntersector;

// 2 ideas:
// 1. we can cache the current lookahead, advance takes a terminal as follows:
//  - narrow down to a terminal that makes the choice
//  - any subsequent narrowing simply returns this terminal
//  - the terminal is not cleared until Advance(terminal) is called.
//  - how to deal with 1 lookahead? cache 2?
//
// 2. EOF should become a special terminal, EofTerminal.
// 3. Having introduced more general terminals gives us a problem where our current algorithm fails and lookahead is insufficient.
//    eg /a/ vs /attribute::* (or to select a node //attribute/...) etc.
//    To solve this we need to introduce a general terminal, this terminal is a regular expression.
//    In order to be able to deal with lookahead, we need to be able to tell if 2 regular expressions have an intersection.
//    This is needed as we can not simply produce a general lookahead table, as we may have conflicts, eg 'a' '::' | 'b' '::', in this case
//    no lookahead table is needed as the prefixes do not intersect. Proposed algorithm:
//  a. for each current terminal, see if it's a match (we could do this by generating a single regex, marking each with a named group).
//  b. if 0 matches: parse error
//     if 1 match: ok, choose
//     if >= 2 matches: lookahead needed, so perform parsing of lookahead terminals (regexes).
//       if >= 2 match < invalid operation, should have been detected by not allowing an intersection
//       if 1 match: ok, got our lookahead -> parse
//       if 0 matches: parse error

// Known issues:
// 1. / vs //.., / should accept an EOF terminal as lookahead, how to do with SimpleRegex?

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
            c.AdvanceInterleaved();
            if (c.Position != c.Expression.Length)
               throw new ParseException("expected end of input at {0}", c.Position.ToString());
         };
      }
   }

   public static class ParserExtensions
   {
      private static void RaiseExpectedTerminals(ParseContext ctx, IEnumerable<Terminal> terminals)
      {
         // todo: add positional info
         throw new ParseException("expected one of '{0}', found '{1}'", String.Join(", ", terminals), String.Join<Terminal>(" or ", terminals)); // todo
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

      public static ParseNode Terminal(this String terminalExpression)
      {
         Terminal tTerminal = new GeneralTerminal(terminalExpression);

         return new ParseNode()
         {
            Label = "TERMINAL(" + tTerminal.ToString() + ")",
            GetDecisionTerminals = (level) => {
                  return new[] { tTerminal };
            },
            Parse = c =>
            {
               c.Advance(tTerminal); 

               //// todo: should put terminal advancing in context
               //if (tTerminal is StringTerminal)
               //   c.Advance(((StringTerminal)tTerminal).TerminalString);
               //else
               //   c.Position += 1; // skip char -> todo: move to context, make method etc.
            },
            ParsesTerminal = s => ((GeneralTerminal)s).SemanticEquals(tTerminal) // s.Equals(tTerminal)
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

                  //HashSet<Terminal> tConflictingTerminals = new HashSet<Terminal>();
                  

                  // todo: should we check if terminals of one node can conflict?
                  foreach (Terminal tTerminal in action.DoGetDecisionTerminals(0))
                     tParseTable.AddTerminal(tTerminal, action); // todo: better error handling if key found

                  Dictionary<Terminal, HashSet<ParseNode>> tConflictMap = new Dictionary<Terminal, HashSet<ParseNode>>();
                  Action<Terminal, ParseNode> tAddConflict = (t, n) =>
                  {
                     HashSet<ParseNode> tList;
                     if (!tConflictMap.TryGetValue(t, out tList))
                     {
                        tList = new HashSet<ParseNode>();
                        tConflictMap.Add(t, tList);
                     }

                     tList.Add(n); // hashset?!
                  };

                  foreach (ParseNode tOtherNode in otherActions)
                     foreach (Terminal tTerminal in tOtherNode.DoGetDecisionTerminals(0)) // todo: must do properly
                     {
                        if (tParseTable.ContainsTerminal(tTerminal))
                        {
                           tAddConflict(tTerminal, tParseTable[tTerminal]);
                           tAddConflict(tTerminal, tOtherNode);
                        }
                        else
                           tParseTable.AddTerminal(tTerminal, tOtherNode);
                     }

                  foreach (Terminal tTerm in tParseTable.Terminals)
                     if (tParseTable.HasConflictingTerminal(tTerm))
                     {
                        tAddConflict(tTerm, tParseTable[tTerm]);                        
                     }

                  LookaheadParseNode tLookaheadNode = new LookaheadParseNode(tConflictMap); // new LookaheadParseNode(tConflictingTerminals, tParseTable);

                  // Replace with lookahead node.
                  foreach (Terminal tConflictingTerminal in tConflictMap.Keys)
                     tParseTable[tConflictingTerminal] = tLookaheadNode;

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
               // todo: there is a way we can be much more efficient here.
               // instead of looping through the parsetable, we can lift off of .nets regex engine:
               // create a regex (( terminal 1) | ( terminal 2) | ... ), using named groups to distinguish the regexes.

               
               c.AdvanceInterleaved(); // todo: must integrate into walker EVEN NECESSARY NOW?
               foreach (GeneralTerminal tTerminal in tParseTable.Terminals) // todo: remove anything but the general terminal
                  if (tTerminal.CanAdvance(c)) // todo: combine into optAdvance?
                  {
                     //tTerminal.AdvanceTerminal(c);
                     tParseTable[tTerminal].Parse(c);
                     return;
                  }
               
               RaiseExpectedTerminals(c, tParseTable.Terminals); 
            },
            LookaheadTerminals = (s,l) => {
               if (tParseTable.ContainsTerminal(s))
               {
                  return tParseTable[s].LookaheadTerminals(s, l);
               } 
               return false;

            } , // follow path to where we want lookahead terminals from
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
                  otherNode.Prime(s); // may queue itself, but result is irrelevant..
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
                        // todo: WTF we don't even need a parsetable here (just use capture to node)

                        var tConflictMap = new Dictionary<Terminal, HashSet<ParseNode>>();
                        Action<Terminal, ParseNode> tAddConflict = (t, n) =>
                        {
                           HashSet<ParseNode> tList;
                           if (!tConflictMap.TryGetValue(t, out tList))
                           {
                              tList = new HashSet<ParseNode>();
                              tConflictMap.Add(t, tList);
                           }

                           tList.Add(n); // hashset?!
                        };

                        tParseTable = new ParseTable<ParseNode>();
                        foreach (Terminal tTerminal in node.DoGetDecisionTerminals(0))
                           if (tParseTable.ContainsTerminal(tTerminal))
                           {
                              tAddConflict(tTerminal, node);
                              tAddConflict(tTerminal, tParseTable[tTerminal]);
                           }
                           else
                              tParseTable.AddTerminal(tTerminal, node);

                        // Find conflicting terminals.
                        HashSet<Terminal> tConflictingTerminals = new HashSet<Terminal>();
                        foreach (Terminal tTerminal in tParseTable.Terminals)
                           if (tParseTable.HasConflictingTerminal(tTerminal))
                              tAddConflict(tTerminal, node);
                             // tConflictingTerminals.Add(tTerminal);

                        LookaheadParseNode tLookaheadNode = new LookaheadParseNode(tConflictMap); // new LookaheadParseNode(tConflictingTerminals, tParseTable);

                        // Replace with lookahead node.
                        foreach (Terminal tConflictingTerminal in tConflictMap.Keys) // tConflictingTerminals)
                           tParseTable[tConflictingTerminal] = tLookaheadNode;
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
                  c.AdvanceInterleaved();
                  foreach(GeneralTerminal tTerminal in tParseTable.Terminals)
                     if (tTerminal.CanAdvance(c))
                     {
                        //tTerminal.AdvanceTerminal(c);
                        tParseTable[tTerminal].Parse(c);
                        goto next;
                     }

                  break; // we're done

               next:;
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
      public String CurrentToken; // todo: remove

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
      protected void Advance(String str) 
      {
         if (str == null)
            throw new ArgumentNullException("Advance: str is null");

         AdvanceInterleaved();

         Int32 i;
         for (i = 0; i < str.Length && Position < Expression.Length && str[i] == Expression[Position]; i++, Position++) ;

         if (i != str.Length)
            throw new ParseException("expected '{0}' around position {1}", str, Position.ToString()); // todo: improve with actual pos

         // todo: advance interleaved again?

         //return (CurrentToken = str);
      }

      public void Advance(Terminal term)
      {
         AdvanceInterleaved();

         term.AdvanceTerminal(this);

         //if (term is StringTerminal)
         //   Advance(((StringTerminal)term).TerminalString);
         //else
         //{
         //   Char tChar = Expression[Position];
         //   Position += 1;
         //   if (!term.Matches(tChar, 0))
         //      throw new InvalidOperationException("expected single char terminal match - got " + tChar);
         //}

         AdvanceInterleaved();
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

   public class ParseNode
   {
      public String Label; // used for debugging only todo: can use a class type for each type of node (?)

      public Boolean Optional = false;
      public Func<Int32, Terminal[]> GetDecisionTerminals;
      public Action<ParseContext> Parse;

      //public String FindLookaheadTerminal(String terminal) = s => null;
      public Func<Terminal, List<Terminal>, Boolean> LookaheadTerminals = (s, l) => true; // return true if route is exhausted, false otherwise
      public Func<Terminal, Boolean> ParsesTerminal = s => false; // todo: can't we do better?

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

      public Terminal[] DoGetDecisionTerminals(Int32 pLevel) // todo: rename this and lambda
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
      /*
       * Three terminals A B C may not all intersect, we may have that A and C intersect, and B and C but not A and C.
       * At the moment I'll stick to the simple case where A B C are all assumed to intersect. We can generalize this
       * by looking at intersection groups, and determining which group to go for during parsing (say A&C) and then deciding
       * on the lookahead.
       * */
      //public LookaheadParseNode(IEnumerable<Terminal> pConflictingTerminals, ParseTable<ParseNode> origParseTable)
      public LookaheadParseNode(Dictionary<Terminal, HashSet<ParseNode>> pConflictMap)
      {
         Label = "LOOKAHEAD";
         GetDecisionTerminals = (level) =>
            {
               throw new InvalidOperationException(); // todo: return terminalStr? I think it makes no sense to ask for dec. terminals here.
            };

         // For lookahead we need a map Terminal -> list of Pair(LookaheadTerminals, node)
         // ie choose node if the lookahead terminal were to match after terminal
         var tLookaheadMap = new Dictionary<GeneralTerminal, List<Pair<List<Terminal>, ParseNode>>>();
         foreach (var tItem in pConflictMap)
         {
            foreach (ParseNode tNode in tItem.Value)
            {
               List<Terminal> tLookaheadTerminals = new List<Terminal>();
               tNode.LookaheadTerminals(tItem.Key, tLookaheadTerminals); // get lookahead
               if (tLookaheadTerminals.Count > 0)
               {
                  List<Pair<List<Terminal>, ParseNode>> tList;
                  if (!tLookaheadMap.TryGetValue((GeneralTerminal)tItem.Key, out tList))
                  {
                     tList = new List<Pair<List<Terminal>, ParseNode>>();
                     tLookaheadMap.Add((GeneralTerminal)tItem.Key, tList);
                  }

                  tList.Add(new Pair<List<Terminal>, ParseNode>() { Left = tLookaheadTerminals, Right = tNode });
                  //tLookaheadMap.Add(tItem.Key, new Pair<List<Terminal>, ParseNode>() { Left = tLookaheadTerminals, Right = tNode });
               }
               else
                  throw new Exception("no lookahead terminals");
            }
         }

         // 3. Determine if our lookahead map is unique, every route is unique.
         // probably quite intensive..
         // todo: this checks everything twice, stupid...
         foreach(var tEntry in tLookaheadMap)
         {
            foreach (var tPair in tEntry.Value)
            {
               SimpleRegex tLeftRegex = SimpleRegex.And(tEntry.Key.SimpleRegex,
                  SimpleRegex.Choice(from t in tPair.Left select ((GeneralTerminal)t).SimpleRegex));

               //foreach (GeneralTerminal tOtherTerminal in tLookaheadMap.Keys)
               foreach(var tOtherEntry in tLookaheadMap)
                  if (!Object.ReferenceEquals(tEntry, tOtherEntry)) // todo: could be trouble if these were reused... must be aware
                  {
                     foreach (var tOtherPair in tOtherEntry.Value)
                     {
                        SimpleRegex tRightRegex = SimpleRegex.And(tOtherEntry.Key.SimpleRegex,
                           SimpleRegex.Choice(from t in tOtherPair.Left select ((GeneralTerminal)t).SimpleRegex));

                        // (tLeftRegex.Intersects(tRightRegex))
                        // todo: could consider a sort of global rewrite, but e must know our complete alphabet then.
                        if (SimpleRegex.RewritesIntersect(tLeftRegex, tRightRegex))
                           throw new ParseException("inconclusive lookahead");
                     }
                  }
            }          
         }

        
         /*
         ParseTable<ParseNode> tParseTable = new ParseTable<ParseNode>();
         List<Terminal> tLookaheadTerminals = new List<Terminal>();
         left.LookaheadTerminals(pTerminal, tLookaheadTerminals);
         foreach (Terminal tTerminal in tLookaheadTerminals)
            tParseTable.AddTerminal(tTerminal, left);
         
         tLookaheadTerminals.Clear();
         right.LookaheadTerminals(pTerminal, tLookaheadTerminals);
         foreach (Terminal tTerminal in tLookaheadTerminals)
            tParseTable.AddTerminal(tTerminal, right); // todo as below
         */

         // this was added SINCE I added lookahead logic...
         LookaheadTerminals = (s, l) =>
         {
            // todo: correct?
            Boolean tResult = true;
            foreach (ParseNode tNode in pConflictMap[s])
               tResult = tResult && tNode.LookaheadTerminals(s, l);
            return tResult;
            //if (tParseTable.ContainsTerminal(s))
            //   return tParseTable[s].LookaheadTerminals(s, l);
            //return false;
         };

         Parse = c =>
            {
               // todo: every time we get here, we already know which terminal advanced, can we use this?

               Int32 tPosition = c.Position;

               foreach (GeneralTerminal tTerminal in tLookaheadMap.Keys)
               {
                  if (tTerminal.CanAdvance(c))
                  {
                     tTerminal.AdvanceTerminal(c); // takes care of interleaving

                     // Choose a lookahead terminal, and then proceed.
                     foreach(var tPair in tLookaheadMap[tTerminal])
                        foreach(GeneralTerminal tLookaheadTerminal in tPair.Left)
                           if (tLookaheadTerminal.CanAdvance(c))
                           {
                              // don't need to advance: we got a hit
                              //tLookaheadTerminal.AdvanceTerminal(c);

                              // NOW we're in business.
                              c.Position = tPosition; // reset for actual parsing (todo: this is a shame)
                              tPair.Right.Parse(c);
                              return; // gtfo
                           }

                     // Failure, reset position, try next terminal.
                     c.Position = tPosition;
                  }
               }

               throw new ParseException("parse error"); // todo: improve error -> how, maybe name terminals (defaulting to strings?, reflection?)
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

   public abstract class Terminal
   {
      //public abstract Boolean Matches(Char pChar, Int32 pPosition);

      public abstract Boolean CanAdvance(ParseContext ctx);
      public abstract String AdvanceTerminal(ParseContext ctx);
      public abstract Terminal Clone();

      public abstract Boolean ConflictsWith(Terminal pOther);
   }

#if false
   public class EofTerminal : Terminal
   {
      public override bool Matches(char pChar, int pPosition)
      {
         return true;
      }

      public override bool ConflictsWith(Terminal pOther)
      {
         return (pOther is EofTerminal);
      }

      public override string ToString()
      {
         return "EOF";
      }

      public override bool Equals(object obj)
      {
         return obj is EofTerminal;
      }

      public override int GetHashCode()
      {
         return 42;
      }
   }
#endif

   public class GeneralTerminal : Terminal
   {
      protected String mExpression;
      protected Regex mRegex;
      protected SimpleRegex mSimpleRegex;

      public SimpleRegex SimpleRegex { get { return mSimpleRegex; } }

      public GeneralTerminal(String expr)
      {
         mSimpleRegex = SimpleRegex.Parse(expr);
         mRegex = new Regex(expr, RegexOptions.CultureInvariant); // todo: options
         mExpression = expr;
      }

      public override bool CanAdvance(ParseContext ctx)
      {
         // todo: must check we started at position...
         return mRegex.IsMatch(ctx.Expression, ctx.Position);
      }

      public override Terminal Clone()
      {
         return new GeneralTerminal(mExpression);
      }

      public override String AdvanceTerminal(ParseContext ctx)
      {
         Match tFirstMatch = mRegex.Match(ctx.Expression, ctx.Position);
         if (!tFirstMatch.Success)
            throw new ParseException("failed to advance terminal"); // todo: improve
         ctx.Position += tFirstMatch.Length;
         return tFirstMatch.Value;
      }

      public override bool ConflictsWith(Terminal pOther)
      {
         if (pOther is GeneralTerminal)
         {
            //return mSimpleRegex.Intersects(((GeneralTerminal)pOther).mSimpleRegex);
            SimpleRegex tLeft = mSimpleRegex;
            SimpleRegex tRight = ((GeneralTerminal)pOther).mSimpleRegex;
            SimpleRegex.Rewrite(ref tLeft, ref tRight);
            //return tLeft.Intersects(tRight);
            return tLeft.SharesCommonPrefixWith(tRight);
         }
         else
            throw new InvalidOperationException("remove other terminals");
      }

      public override string ToString()
      {
         return mExpression;
      }

      //// We need this for Terminal.ParsesTerminal etc.
      public bool SemanticEquals(object obj)
      {
         GeneralTerminal tOther = obj as GeneralTerminal;
         if (tOther == null)
            return false;

         if (tOther.mExpression == mExpression) // todo: case for insensitivity
            return true;

         if (SimpleRegex.RewritesEqual(this.mSimpleRegex, tOther.mSimpleRegex))
            return true;

         return false;
      }

      public override bool Equals(object obj)
      {
         return SemanticEquals(obj); // todo: if this ok, can remove of course
      }

      public override int GetHashCode()
      {
        // return this.mSimpleRegex.EqualsConsistentHashCode(); // todo: may wish to try 
         return 42; // todo: use above, but make sure that bugs in the above are avoided first
      }
   }

#if false

   public class StringTerminal : Terminal
   {
      protected String mTerminalStr;

      public String TerminalString { get { return mTerminalStr; } }

      public StringTerminal(String terminalStr)
      {
         if (String.IsNullOrEmpty(terminalStr))
            throw new ArgumentException("terminal string is null or empty");
         mTerminalStr = terminalStr;
      }

      public override bool Matches(char pChar, int pPosition)
      {
         return (pPosition < mTerminalStr.Length && mTerminalStr[pPosition] == pChar); // || pPosition >= mTerminalStr.Length;
      }

      public override int GetHashCode()
      {
         return mTerminalStr.GetHashCode();
      }

      public override bool Equals(object obj)
      {
         StringTerminal tOther = obj as StringTerminal;
         return tOther != null && tOther.mTerminalStr == mTerminalStr; // todo: equals?
      }

      public override string ToString()
      {
         return "\"" + mTerminalStr + "\"";
      }

      public override bool ConflictsWith(Terminal pOther)
      {
         if (pOther is StringTerminal)
            return ((StringTerminal)pOther).mTerminalStr == mTerminalStr;

         if (pOther is RangeTerminal || pOther is ChoiceTerminal)
            // Conflict only if we are 1 length, otherwise narrowing will continue.
            return mTerminalStr.Length == 1 && pOther.Matches(mTerminalStr[0], 0);

         throw new InvalidOperationException("unknown terminal type");

         //switch (pOther.GetType())
         //{
         //   case typeof(StringTerminal):
         //      break;

         //   case typeof(RangeTerminal):
         //      break;

         //   case typeof(ChoiceTerminal):
         //      break;

         //   case typeof(NegationTerminal):
         //      break;

         //   default:
         //      throw new InvalidOperationException("unknown terminal type");
         //}
      }
   }

   public class RangeTerminal : Terminal
   {
      public Char Low { get; protected set; }
      public Char High { get; protected set; }

      public RangeTerminal(Char lower, Char higher)
      {
         Low = lower;
         High = higher;
      }

      public override bool Matches(char pChar, int pPosition)
      {
         if (pPosition != 0)
            return false;

         return pChar >= Low && pChar <= High;
      }

      public override int GetHashCode()
      {
         return Low.GetHashCode() ^ High.GetHashCode();
      }

      public override bool Equals(object obj)
      {
         RangeTerminal tOther = obj as RangeTerminal;
         return tOther != null && tOther.Low == Low && tOther.High == High;
      }

      public override string ToString()
      {
         return "[" + Low + "-" + High + "]";
      }

      public override bool ConflictsWith(Terminal pOther)
      {
         if (pOther is RangeTerminal)
         {
            RangeTerminal tOther = (RangeTerminal)pOther;
            return (tOther.Low >= this.Low && tOther.Low <= this.High) || (tOther.High <= this.High && tOther.High >= this.Low);
         }

         return pOther.ConflictsWith(this);
      }
   }

   public class ChoiceTerminal : Terminal
   {
      public Char[] Chars;
      public Boolean Negated;

      public ChoiceTerminal(Boolean pNegate, params Char[] chars)
      {
         if (chars.Length == 0)
            throw new ArgumentException("no chars in choice terminal");

         Chars = chars; // copy?
         Negated = pNegate;
      }

      public override bool Matches(char pChar, int pPosition)
      {
         for (Int32 i = 0; i < Chars.Length; i++)
            if (pChar == Chars[i])
               return true && !Negated;

         return Negated;
      }

      public override int GetHashCode()
      {
         return Negated.GetHashCode() ^ Chars.GetHashCode();
      }

      public override bool Equals(object obj)
      {
         ChoiceTerminal tOther = obj as ChoiceTerminal;
         return tOther != null && tOther.Negated == this.Negated && Array.Equals(tOther.Chars, this.Chars);
      }

      public override string ToString()
      {
         return "[" + (Negated ? "^" : "") + String.Join("", Chars) + "]";
      }

      public override bool ConflictsWith(Terminal pOther)
      {
         if (pOther is StringTerminal)
            ((StringTerminal)pOther).ConflictsWith(this);

         if (Negated)
         {
            return true;
         }
         else
         {
            foreach (Char tChar in Chars)
               if (pOther.Matches(tChar, 0))
                  return true;
            return false;
         }
      }
   }

   #endif

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
      protected Dictionary<Terminal, T> Table = new Dictionary<Terminal, T>();

      public void AddTerminal(String terminalStr, T node)
      {
         AddTerminal(new GeneralTerminal(terminalStr), node);
      }

      public void AddTerminal(Terminal pTerm, T node) 
      {
         if (pTerm == null)
            throw new ArgumentException("key empty or null");

         if (node == null)
            throw new ArgumentException("value is null");

         Table.Add(pTerm, node);
      }

      public Boolean ContainsTerminal(Terminal pTerminal)
      {
         return Table.ContainsKey(pTerminal);
      }

      // todo: do this properly
      public T GetMatchingTerminal(Terminal pTerminal)
      {
         //return Table.Keys.Any(k => k == pTerminal ||  ((GeneralTerminal)k).SemanticEquals(pTerminal));
         var tResult = from k in Table.Keys where k == pTerminal || ((GeneralTerminal)k).SemanticEquals(pTerminal) select this[k];
         if (tResult.Count() > 1)
            throw new Exception("todo");
         return tResult.FirstOrDefault();
      }

      public Boolean HasConflictingTerminal(Terminal pTerminal)
      {
         //Boolean tResult = Table.Any(t => pTerminal.ConflictsWith(t.Key));
         //return tResult;

         foreach (Terminal tTerminal in Table.Keys)
            if (tTerminal != pTerminal && tTerminal.ConflictsWith(pTerminal))
               return true;

         return false;
      }

      public T this[Terminal pTerm]
      {
         get { return Table[pTerm]; }
         set 
         {
            if (value == null)
               throw new ArgumentNullException();

            Table[pTerm] = value; 
         }
      }

      public Boolean TryGetNode(Terminal pTerm, out T node)
      {
         return Table.TryGetValue(pTerm, out node);
      }

      public IEnumerable<Terminal> Terminals { get { return Table.Keys; } }

#if false

      public Walker GetWalker()
      {
         return new Walker(Table, -1);
      }

      public class Walker
      {
         public Walker(Dictionary<Terminal, T> pTable, Int32 pMaxLength) 
         {
            mTable = pTable;
            mMatchSet = new HashSet<Terminal>();
            foreach (Terminal tTerminal in mTable.Keys)
               mMatchSet.Add(tTerminal);

            MaxLength = pMaxLength;
         }

         protected HashSet<Terminal> mMatchSet;
         protected Dictionary<Terminal, T> mTable;
         protected Boolean mNarrowed = false;

         public Int32 Count = 0;
         public Int32 NumberRead = 0;
         public Int32 MaxLength;

         public Boolean Narrows(Char pChar)
         {
            Int32 tCount;
            return Narrows(pChar, out tCount);
         }

         // todo: I have taken out a case of a shared prefix, e.g. foo and foobar. Must add back in.
         // Returns true if the char narrows the search, but is not yet unambiguous.
         // Returns false if no results or a single result remain.
         // True indicates the search narrows but is not yet finished.
         // todo: inefficient
         public Boolean Narrows(Char pChar, out Int32 tCount)
         {
            tCount = mMatchSet.Count;
            
            if (mNarrowed && mMatchSet.Count <= 1)
               return false;

            mNarrowed = true; // indicates we have narrowed, may not happen if, say, eof was reached

            Terminal[] tMatchTerms = mMatchSet.ToArray(); // todo: avoid this

            foreach (Terminal tKey in tMatchTerms)
               if (!tKey.Matches(pChar, NumberRead))
               {
                  mMatchSet.Remove(tKey);
               }

            tCount = mMatchSet.Count;
            NumberRead += 1;

            return mMatchSet.Count > 1;
         }

         // assumes things have narrowed...
         public T Value
         {
            get
            {
               if (mMatchSet.Count == 0 || !mNarrowed)
                  return null;
               if (mMatchSet.Count > 1)
                  throw new InvalidOperationException("must narrow");

               return mTable[mMatchSet.First()];
            }
         }
      }
#endif
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
