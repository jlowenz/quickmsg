import quickmsg.*;

class AsyncSubscriberImpl extends AsyncSubscriber {
    public AsyncSubscriberImpl(String topic)
    {
        super(topic);
    }
    
    @Override
    public void handle_message(Message m) 
    {
        System.out.println("Java inherited subscriber callback");
        System.out.println(m.get_msg());
    }

}

class SubscriberImpl extends Subscriber {
    public SubscriberImpl(String topic)
    {
        super(topic);
    }
}

public class test_sub {
    static {
        String lib_name = "quickmsg_java";
        try {
            System.loadLibrary(lib_name);
        } catch (UnsatisfiedLinkError e) {
	    e.printStackTrace();
            System.err.println("Could not find "+lib_name+". Check Library Search Path");
            System.exit(1);
        }
    }

    public static void main(String[] argv) {
        // By default, qm uses whatever interface it finds finds first.
        // this should be tailored to network configuration
        String iface = "";
	quickmsg_java.init("test_java_sub", iface);
	Runtime.getRuntime().addShutdownHook(new Thread() {
		@Override
		public void run()
		{
		    System.out.println("shutdown hook");
		    quickmsg_java.shutdown("done");
		}
	    });

        AsyncSubscriberImpl sub = new AsyncSubscriberImpl("chatter");
	sub.spin();
	sub.delete();
	quickmsg_java.shutdown("");
        // AsyncSubscriberImpl asub = new AsyncSubscriberImpl("chatter");
        // asub.spin();
    }
}

