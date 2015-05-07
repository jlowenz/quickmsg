class AsyncSubscriberImpl extends AsyncSubscriber {
    public AsyncSubscriberImpl(String topic)
    {
        super(topic);
    }
    
    public void subscriber_impl(Message m) 
    {
        System.out.println("Java inherited subscriber callback");
        System.out.println(m.getMsg());
    }

}

class SubscriberImpl extends Subscriber {
    public SubscriberImpl(String topic)
    {
        super(topic);
    }

    public void subscriber_impl(Message m) 
    {
        System.out.println("Java inherited subscriber callback");
        System.out.println(m.getMsg());
    }    

}

public class test_sub {
    static {
        String lib_name = "quickmsg_java";
        try {
            System.loadLibrary(lib_name);
        } catch (UnsatisfiedLinkError e) {
            System.err.println("Could not find "+lib_name+". Check Library Search Path");
            System.exit(1);
        }
    }

    public static void main(String[] argv) {
        SubscriberImpl sub = new SubscriberImpl("chatter");
        while(true) {
            try{
                Thread.sleep(100);
            } catch(InterruptedException e){
                System.out.println("got interrupted!");
            }
        }
        // AsyncSubscriberImpl asub = new AsyncSubscriberImpl("chatter");
        // asub.spin();
    }
}

