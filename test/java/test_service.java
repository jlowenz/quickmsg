//import org.json.simple.*;
import java.util.*;
import quickmsg.*;

class HelloService implements IServiceCallback
{       
    private int count = 0;
    public String handleMessage(Message m)
    {
	System.out.println("Got: " + m.get_msg());
	return "World " + Integer.toString(count);
    }
}

public class test_service {
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
	quickmsg_java.init("test_java_service");
        Service svc = new Service("hello", new HelloService());
        svc.spin();
	svc.delete();
    }
}

