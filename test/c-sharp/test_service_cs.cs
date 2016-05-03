using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using quickmsg;


class ServiceCS : Service
{
    int msgs_recvd;

    public ServiceCS(string top, uint queue_size)
        : base(top, queue_size)
    {
        msgs_recvd = 0;
    }

    public override string service_impl(Message req)
    {
        Console.WriteLine("bj_got: " + req.get_msg());
        msgs_recvd++;
        return "World " + msgs_recvd;
    }
}



class test_service_cs
{
    static void Main(string[] args)
    {
	string iface = "";
	quickmsg_csharp.init("serv work", iface);
	ServiceCS serv = new ServiceCS("hello", 20);
	serv.spin();
    }
}

