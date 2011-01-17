using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace SimpleCC
{
   public class Parser
   {
      public void LoadBnf(String pBnfFile)
      {
         if (!File.Exists(pBnfFile))
            throw new SimpleCCException("BNF definitions file '{0}' not found.", pBnfFile);


      }
   }
}
