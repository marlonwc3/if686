import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Teste {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		Lock l = new ReentrantLock();
		l.lock();
		
		l.unlock();
		
		
	}

}
