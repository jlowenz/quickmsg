import quickmsg.*;

public class test_pub {
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
	quickmsg_java.init("test_pub");
        Publisher p = new Publisher("chatter");
        String s = "Java Publisher Hello!";
        for (int i=0; i < 10; i++) {
            if (!quickmsg_java.ok()) {
                break;
            }
            System.out.println("Publishing: "+s);
            p.publish(s);
            try{
                Thread.sleep(100);
            } catch(InterruptedException e){
                System.out.println("got interrupted!");
            }
        }
    }
}

