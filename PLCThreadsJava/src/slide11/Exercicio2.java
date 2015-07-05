package slide11;

import java.util.ArrayList;

class Contador2 extends Thread {
	ArrayList<Long> primes;
	long l, r;
	int id;
	public Contador2(){}
	public Contador2(long l, long r, int id){
		this.l = l; this.r = r;
		this.id = id;
		this.primes = new ArrayList<>();
	}
	
	 public void run(){
		for(long i = l; i <= r; i++){
			if(Exercicio2.isPrime(i)) primes.add(i);
			
		}
	}
	@Override
	public String toString() {
		return "Contador2 [ id: " + id  + ", l=" + l + ", r=" + r + "]" + "primes=" + primes;
	}
	void printResult(){
		System.out.println("Thread ("+id+") obteve: " + this.primes);
	}
}


public class Exercicio2 {
	public static boolean isPrime(long n){
		if(n<=3) return n >=2; 
		for(int i = 2; i*i <= n ; i++){
			if( n%i == 0 ) return false;
		}
		return true;
	}
	public static void main(String[] args) {
		long N = 10000000;
		int NUM_THREADS = 10;
		long SLICE = N / NUM_THREADS, L = 0;
		ArrayList<Contador2> threads = new ArrayList<>();
		for(int i =0 ; i < NUM_THREADS; i++){
			threads.add(new Contador2(L, (i == NUM_THREADS-1) ? N : L+SLICE  , i) );
			System.out.println(threads.get(i));
			threads.get(i).start();
			L += SLICE+1;
		}
		try { 
			for( int i = 0; i < NUM_THREADS; i++ ) {
				threads.get(i).join();
			}
			for(int i=0 ; i < NUM_THREADS; i++){
				threads.get(i).printResult();
			}
		}
		catch(InterruptedException e){
			e.printStackTrace();
		}
		
	}
	
	
	
	
}















