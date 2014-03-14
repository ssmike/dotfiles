import java.io.PrintWriter;
import java.io.BufferedReader;
import java.io.BufferedInputStream;
import java.util.StringTokenizer;
import java.math.BigInteger;
import java.io.InputStreamReader;

public final class Main {
    BufferedReader inp = null;
    StringTokenizer buf = null;
    PrintWriter outp = null;
    
    String getToken() throws Exception {
        while (buf == null || !buf.hasMoreTokens()) 
            buf = new StringTokenizer(inp.readLine());
        return buf.nextToken();
    }

    String getLine() throws Exception {
        if (buf != null) {
            String b = buf.toString();
            buf = null;
            return b;
        }
        return inp.readLine();
    }

	void run() throws Exception {
        // beginning of code
        
        // end of code
    }

    public static void main(String[] args) throws Exception {
        Main m = new Main();
        m.inp = new BufferedReader(new InputStreamReader(System.in));
        m.outp = new PrintWriter(System.out);
		m.run();
        m.outp.close();
        m.inp.close();
	}
}
