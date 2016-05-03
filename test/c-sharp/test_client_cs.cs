using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using quickmsg;

class test_client_cs
{
    static void Main(string[] args)
    {
	string iface= "";
	quickmsg_csharp.init("test_cs_client", iface);
	Client cl = new Client("hello");

	string req = "Hello";
	for (int i = 0; i < 10; ++i)
	{
	    if (!quickmsg_csharp.ok()) break;
	    Console.WriteLine("client request\n" + req);
	    string resp = cl.calls(req);
	    Console.WriteLine("server response: " + resp);
	    System.Threading.Thread.Sleep(1000);
	}
    }
}

