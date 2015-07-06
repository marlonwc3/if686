package slide12;

import java.util.ArrayList;
import java.util.Queue;
import java.util.Random;
import java.util.Scanner;
import java.util.Vector;
import java.util.concurrent.atomic.AtomicBoolean;

class T1 extends Thread{
	private int id;
	public boolean add = true; 
	private Random r = new Random();;
	T1(){} 
	T1(int id){this.id = id;}
	public void run(){
		 for(int i = 0 ; i < 10 ; i++ ) {
			 if(add) Exercicio2.s.push(r.nextInt(10000)+17, id);
			 else try { Exercicio2.s.pop(); } catch(Exception e){} 
		 }
		
	}
}


class SafeQueueWithoutSync {
	Vector<T1> threads = new Vector<>(); 
	private ArrayList<Objeto> queue = new ArrayList<>();
	private AtomicBoolean isBusy = new AtomicBoolean(false);
	public static String pushed = "", popped = "";
	public void push(int s, int id){
		while(isBusy.getAndSet(true)){ }
		//System.out.println("Thread  ("+id+") inseriou val: " + s);
		this.queue.add(new Objeto(s, id) );
		pushed += "[T + " +id +"|" + s +"]#";
		isBusy.set(false);
	}
	public int pop() throws Exception{
		while(isBusy.getAndSet(true)){}
		if(this.queue.isEmpty()) throw new Exception("Lista vazia");
		Objeto o = this.queue.remove(0);
		popped += "[T + " + o.t +"|" + o.x +"]#";
		isBusy.set(false);
		return o.x;
	}
}

public class Exercicio2 extends Thread{
	static SafeQueueWithoutSync s = new SafeQueueWithoutSync();

	public static void main(String[] args) {
		int N = 30;
		for(int i = 0; i < N; i++) {
			s.threads.addElement(new T1(i));
			s.threads.lastElement().start();
		}
		for(int i = 0; i < N; i++) { 
			try { s.threads.get(i).join(); } catch(Exception e){e.printStackTrace();} 
		}
		for(int i = 0; i < N; i++) { s.threads.set(i, new T1(i)); s.threads.get(i).add = false;  s.threads.get(i).start(); }		
		for(int i = 0; i < N; i++) { 
			try { s.threads.get(i).join(); s.threads.get(i).add = false; } catch(Exception e){e.printStackTrace();} 
		}
		System.out.println(s.pushed);
		System.out.println(s.popped);
		System.out.println(s.pushed.equals(s.popped));
	}
}
