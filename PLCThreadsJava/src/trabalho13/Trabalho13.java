package trabalho13;

import java.util.Random;
import java.util.Vector;
import java.util.concurrent.atomic.AtomicBoolean;


class Runner extends Thread{
	int id;
	public Runner(){}
	public Runner(int id){this.id = id; }
	public void run(){
		int x; 
		for(int i = 0; i < 3; i++) {
			x = Trabalho13.manager.getTicket(id);
			System.out.println("Thread ("+id+") pegou ticket: " + x );
		}
	}
}

class TicketWorker extends Thread{
	private TicketManager manager; 
	Random random = new Random(); 
	int[] queries;
	public TicketWorker(TicketManager manager){ 
		this.manager = manager;
		queries = new int[manager.N+2]; 
	}
	private void myShuffle(int N){
		int pos; 
		for(int i = N; i > 0; i-- ){
			pos = random.nextInt(i);
			if(pos != i-1 ) {
				int aux = queries[i-1];
				queries[i-1] = queries[pos]; 
				queries[pos] = aux; 
			}
		}			
	}
	public void run() {
		int NEXT = 0; 
		int p; 
		while(true){
			for(int i = 0; i < this.manager.N; i++){
				try{
					if(manager.requisitions[i].get() ) {
						queries[NEXT++]= i;
					}
				}catch(Exception e ) {
					e.printStackTrace();
				}
			}
			if(NEXT>0){
				myShuffle(NEXT);
				for(int i = 0; i < NEXT ; i++ ){
					p = this.queries[i]; 
					manager.numbers[p] = this.manager.NEXT_TICKET++;
					manager.requisitions[p].set(false);
				} 
			}
			NEXT= 0;
		}
	}
}

class TicketManager {
	public Vector<Runner> threads = new Vector<>(); 
	public int N = 0; // Threads number
	public int NEXT_TICKET = 0 ;
	public int[] numbers = new int[105];
	public	AtomicBoolean[] requisitions = new AtomicBoolean[105];
	private TicketWorker worker; 
	public TicketManager(int N, Vector<Runner> threads ){
		this.N = N ;
		this.threads = threads; 
		for(int i =0 ; i <= N; i ++ ) { 
			this.requisitions[i] = new AtomicBoolean();
		} 
		this.worker = new TicketWorker(this);
		this.worker.start();
	}

	int getTicket(int id){
		this.requisitions[id].set(true);
		while(requisitions[id].get() ) { }
		int ticket = this.numbers[id]; 
		return ticket; 
	}
	void stopWork(){
		this.worker.stop();
	}
} 



public class Trabalho13 {
	static TicketManager manager; 
	public static void main(String[] args) {
		Vector<Runner> threads = new Vector<>();
		int N  = 20; 
		for(int i = 0 ; i < N ; i ++ ) { threads.add(new Runner(i)); }
		manager = new TicketManager(N, threads);
		for(int i  =0 ; i < N; i++ ){
			manager.threads.get(i).start();
		}
		for(int i  =0 ; i < N; i++ ){
			try {				
				manager.threads.get(i).join();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}		
		manager.stopWork(); 		
	}
}
