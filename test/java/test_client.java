//import org.json.simple.*;
import quickmsg.*;

public class test_client {
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
	quickmsg_java.init("test_java_client");
        Client c = new Client("hello");
	System.out.println("after client creation");
	try {
	    for (int i = 0; i < 10; i++) {
		String response = c.calls("Hello");
		System.out.println("Client response "+response);
		Thread.sleep(500);
	    }
	} catch (Exception e) {
	    e.printStackTrace();	   
	} finally {
	    c.delete();
	}
    }
}

