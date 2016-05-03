using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using quickmsg;

class SubscriberCS : AsyncSubscriber {
    public SubscriberCS(string m) : base(m) { }

    public override void handle_message(Message msg){
        Console.WriteLine("bj_msg: " + msg.get_msg());
    }//handle_message
}//SubscriberCS


class test_sub_cs{
    static void Main(string[] args){

        string iface = "";
        quickmsg_csharp.init("sub work", iface);

        SubscriberCS sub_cs = new SubscriberCS("chatter");
        sub_cs.spin();
        sub_cs.Dispose();
    }//Main
}//class
