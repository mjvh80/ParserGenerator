using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SimpleCC
{
   public class SimpleCCException : Exception
   {
      public SimpleCCException(String msg) : base(msg) { }
      public SimpleCCException(String format, params Object[] args) : base(String.Format(format, args)) { }
   }
}
