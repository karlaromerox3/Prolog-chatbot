package chatbot.pl;
import java.util.Hashtable;
import org.jpl7.*;
import org.jpl7.Query;
/**
 *
 * @author Karla Romero
 */
public class Chatbot {
        public static void main( String argv[] )
        {
        String t1 = "consult('bot_logic.pl')";
        Query q1 = new Query(t1);
        System.out.println( t1 + " " + (q1.hasSolution() ? "correcto" : "fallo") );
        String t2 = "beginChat.";
        Query q2 = new Query(t2);
        System.out.println( t2 + " is " + (q2.hasSolution() ? "probado" : "no probado") );
       
        }
 }

