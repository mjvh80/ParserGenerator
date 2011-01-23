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
      class Foo { public Int32 Bar = 1; }

      static void Main(String[] args)
      {
         Foo foo = new Foo();
         Action a = () => { Console.WriteLine(foo.Bar); };
         a();
         List<Foo> list = new List<Foo>();
         list.Add(foo);
         Two(list);
         a();
         Console.Read();
      }

      static void Two(ref Foo foo) { foo.Bar = 2; }
      static void Two(List<Foo> list) { list[0].Bar = 2; }

      static void Main2(string[] args)
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
