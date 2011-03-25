using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SimpleCC
{
   /// <summary>
   /// Implements a BNF parser generator, using our parser generator.
   /// </summary>
   public class BnfParser
   {
#if todo
      protected class ActualParser : ParserBase
      {
         // todo: need context with whitespace handling

         protected override void DefineGrammar()
         {
            ParseNode Production = null, Name = null, Definition = null;

            Root = Rule(() => Production.EOF());

            Production = Rule(() => Name.FollowedBy("::=".FollowedBy(Definition)));

            // need string here..
            Name = "foo".Terminal();


            // flatten

            // compile
            // 

         }
      }
#endif

      public BnfParser()
      {

      }
   }
}
