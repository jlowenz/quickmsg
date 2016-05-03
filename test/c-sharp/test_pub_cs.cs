using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using quickmsg;


class test_pub_cs
{
    static void Main(string[] args)
    {		
	Publisher p;
	string iface = "";
	quickmsg_csharp.init("work", iface);
	p = new Publisher("chatter");

	for (int i = 0; i < 10; i++)
	{

	    string msg = "'ello govnuh " + i;
	    p.publish(msg);

	    System.Threading.Thread.Sleep(1000);
	}
	p.Dispose();
	quickmsg_csharp.shutdown();
    }
}
