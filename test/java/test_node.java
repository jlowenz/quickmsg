//import org.json.simple.*;
import java.util.*;
import quickmsg.*;

class ServiceHandler implements IMessageCallback
{
    private GroupNode node;
    private int count = 0;

    ServiceHandler(GroupNode node)
    {	
	this.node = node;
    }

    public void handleMessage(Message m)
    {
	System.out.println("Got request: " + m.get_msg());
	count += 1;
	node.whispers(m.get_src(), "World " + Integer.toString(count));
    }
}

public class test_node {
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
	quickmsg_java.init("test_java_node");
	GroupNode node = new GroupNode("SRV/svc_node", true);
	node.join("hello");
	node.register_handler("hello", new ServiceHandler(node));
	node.async_spin();
	node.join();	
	node.delete();
    }    
}
