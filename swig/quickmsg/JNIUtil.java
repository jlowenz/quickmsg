package quickmsg;

// we assume this is ALREADY available!
//import clojure.lang.RT;


// TODO: this could be a possible problem for non-Clojure quickmsg
// Java development Need to look into a way to specify using quickmsg
// with Clojure or not to avoid this function. Or at least only run it
// once per thread.

public class JNIUtil
{
    public static void load_classloader()
    {
	try {
	    ClassLoader cl = JNIUtil.class.getClassLoader();
	    Class rt = cl.loadClass("clojure.lang.RT");
	    Thread.currentThread().setContextClassLoader(rt.getClassLoader());
	    System.out.println("Set contextClassLoader for CLOJURE");
	} catch (ClassNotFoundException cnf) {
	    System.out.println("ContextClassLoader not set. This is OK if you are not using Clojure");
	} catch (Throwable t) {
	    System.out.println("Failed to set thread classloader :-(");
	}
    }
}
