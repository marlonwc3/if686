package slide12;

import java.util.ArrayList;
import java.util.Queue;
import java.util.Random;
import java.util.Scanner;
import java.util.Vector;
import java.util.concurrent.atomic.AtomicBoolean;

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
class T1 extends Thread{
	static SafeQueueWithSync s = new SafeQueueWithSync();
	private int id;
	public T1(int id){this.id= id;}
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
} 


class SafeQueueWithoutSync {
	public Vector<T2> threads;
	private ArrayList<Objeto> queue = new ArrayList<>();
	public AtomicBoolean canAcess; 
	public SafeQueueWithoutSync(Vector<T2> threads) {
		this.threads = threads;
	}
	
	public void push(int s, int id){
		while(canAcess.getAndSet(true)){
			try {
				this.threads.get(id).sleep(1000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		System.out.println("Thread  ("+id+") inseriou val: " + s);
		this.queue.add(new Objeto(s, id) );
		canAcess.set(false);
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
class T2 extends Thread{
	static SafeQueueWithSync s = new SafeQueueWithSync();
	private int id;
	public T2(int id){this.id= id;}
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
} 


public class TestePriorityAcess extends Thread{

	public static void main(String[] args) {
		int N = 5;
		Vector<T1> v = new Vector<>();
		for(int i = 0; i < N; i++) {
			v.addElement(new T1(i));
			v.lastElement().start();
		}
		for(int i = 0; i < N; i++) try{v.get(i).join();}catch(Exception e) {e.printStackTrace();}
		
		
		
	
	}
}
