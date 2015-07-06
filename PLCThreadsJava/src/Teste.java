import java.util.PriorityQueue;
import java.util.Random;
import java.util.Vector;
import java.util.concurrent.atomic.AtomicBoolean;

class T1 extends Thread{ 
	int id, x;
	public T1(){}
	public T1(int id, int x){this.id = id; this.x = x;  }
	public void run(){
		Teste.acess(x, id);
	}
}

public class Teste {
	static PriorityQueue<Integer> heap = new PriorityQueue<>();
	static int NEXT = 1;
	static String strIn="", strOut="", strMid ="";
	
	// Threads entraram na região B na mesma ordem em que entraram em A
	static private int tryAcess(int id, int myNext){
		
		if(myNext == -1 ) { 
			synchronized (Teste.class) {  // Região A
				 myNext = NEXT++;
				 heap.add(myNext);
				 strIn += "T["+id+"]"+"#";
			}
		}	
		
		// região não crítica 

		synchronized (Teste.class) {  // Região B
			synchronized(heap) {  
				int boy = heap.element(); 
				if(boy != myNext) return myNext; 
				heap.remove();
			};
			strOut += "T["+id+"]"+"#";
		}
		return 0;
	}

	static public void acess(int x, int id) {
		int myNext = -1;
		while(myNext != 0 ){
			myNext = tryAcess(id, myNext);
		}
	}
	
	public static void main(String[] args) {
		int N = 500;
		Vector<T1> threads = new Vector<>();
		Random r = new Random();
		for(int i = 0 ; i < N; i++){
			threads.add(new T1(i, r.nextInt(50) + 50));
			threads.lastElement().start();
		}
		for( int i = 0; i < N; i++) try{threads.get(i).join();} catch(Exception e){e.printStackTrace();}
		//System.out.println(strIn);
		//System.out.println();
		//System.out.println(strOut);
		System.out.println("Iguais: " + strIn.equals(strOut));
		
	}

}


