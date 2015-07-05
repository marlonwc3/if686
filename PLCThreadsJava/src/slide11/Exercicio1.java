package slide11;
import java.util.ArrayList;

class Contador1 implements Runnable{
	int l, r; 
	public Contador1(int _l, int _r){ 
		l=_l; r=_r;
		this.run();
	}
	public void run() {
		for (int i = l; i <= r; i++) {
			System.out.println(i);
		}
	}
	@Override
	public String toString() {
		return "Contador1 [l=" + l + ", r=" + r + "]";
	}
	
}

public class Exercicio1 {
	public static void main(String[] args) {
		int N=2;
		int SLICE = (2*1000000)/N;
		int L = 0;
		ArrayList<Contador1> threads = new ArrayList<>() ;
		for(int i =0 ; i < N; i++ ) {
			threads.add(new Contador1(L, (i==N-1) ? 2*1000000 : L+SLICE  ));			
			L+=SLICE+1;
		}
	}
}