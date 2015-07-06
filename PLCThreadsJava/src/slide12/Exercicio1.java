package slide12;

import java.util.ArrayList;
import java.util.Queue;
import java.util.Random;
import java.util.Scanner;
import java.util.Vector;

class Objeto{
	int x, t;
	public Objeto(){}
	public Objeto(int x, int t){ this.x = x; this.t = t;}
	public String toString() {
		return("[Objeto , criado pela thread: " + t + " | val: " + x);
	}
}

class SafeQueueWithSync {
	private ArrayList<Objeto> queue = new ArrayList<>();
	public void push(int s, int id){
		synchronized (queue) {
			System.out.println("Thread  ("+id+") inseriou val: " + s);
			this.queue.add(new Objeto(s, id) );
		}
	}
	public Objeto pop() throws Exception{
		synchronized (queue) {
			if(this.queue.isEmpty()) {
				throw new Exception("Fila vazia");
			}
			return this.queue.remove(0);
		}
	}
}

public class Exercicio1 extends Thread{
	static SafeQueueWithSync s = new SafeQueueWithSync();
	private int id;
	public Exercicio1(int id){this.id= id;}
	
	public void run(){
		Random r = new Random();
		for(int i = 0 ; i < 10; i++){
			s.push(r.nextInt(100), id);
		}
		Objeto o;
		for(int i  = 0; i< 10; i++){
			try {
				o = s.pop();
				System.out.println("Thread ("+id+") removeu: " + o);				
			} catch (Exception e) {
				System.out.println("Thread ("+id+") encontrou lista vazia");				
			}
			
		}
	}
	public static void main(String[] args) {
		
		for(int i = 0; i < 5; i++) {
			new Exercicio1(i).start();
		}
		
	
	}
}
