using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Parser;
using XPath2.Parser;

namespace SimpleConsoleTests
{
   class Program
   {
      static void Main(string[] args)
      {
         for(;;)
         {
            Console.Write(">");
            try
            {
               String tLine = Console.ReadLine();
               if (tLine == "!")
                  return;

               XPath2Parser tParser = new XPath2Parser();
               tParser.Parse(tLine);
               Console.WriteLine("OK");
            }
            catch (Exception e)
            {
               Console.WriteLine("ERROR: " + e.Message);
         //      Console.WriteLine(e.StackTrace);
            }
         }
      }
   }
}
