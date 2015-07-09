package slide14;

import java.util.Random;
import java.util.Scanner;
import java.util.Vector;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class T1 extends Thread{
	Vector<Integer> v = new Vector<>();
	int id;
	public T1(){}
	public T1(int id){ this.id = id; }
	public void run(){
		int MAX_POS = Exercicio1.N;
		Random r = new Random();
		int a , b, op ; 
		for(int  i = 0; i < 100; i++ ) {
			op = r.nextInt(3);
			a = r.nextInt(MAX_POS);
			if(op==0){

				b = r.nextInt(100);
				Exercicio1.list.set(a, b);
			}
			else if(op == 1) {
				b = Exercicio1.list.get(a);
				//v.addElement(b);
			}
			else {
				b = r.nextInt(MAX_POS);
				Exercicio1.list.swap(a, b);
			}
			
		}
		//Exercicio1.go1(id);
	}
}

class SafeList{
	int N; 
	int[] list;
	Lock[] listLock;
	SafeList(){N = 0; list = new int[0]; this.listLock = new ReentrantLock[0]; }
	SafeList(int N){ this.N = N; this.list = new int[N]; 
		this.listLock = new ReentrantLock[N];
		for(int i = 0; i < N; i++ ) this.listLock[i] = new ReentrantLock();
	}
	int get(int i){
		int x; 
		while(!listLock[i].tryLock()){ }
		x = list[i];
		listLock[i].unlock();
		return x;
	}
	void set(int i, int v){
		while(!listLock[i].tryLock()){ }
		list[i] = v;
		listLock[i].unlock();
	}
	void swap(int a, int b){
		boolean b1 = false, b2 = false;
		while(!b1 || !b2 ){
			b1 = listLock[a].tryLock();
			b2 = listLock[b].tryLock();
			if(b1 && b2 ) {
				int aux = list[a];
				list[a] = list[b];
				list[b] = aux; 
			}
			if(b1) listLock[a].unlock();
			if(b2) listLock[b].unlock();
			
		}
	}
}

public class Exercicio1{
	static volatile int N = 1000; 
	public static SafeList list = new SafeList(N);
	
	public static void main(String[] args) {
		int NumThreads = 100;
		Vector<T1> threads = new Vector<>();
		for(int i =0 ; i < NumThreads; i++ ) {
			threads.addElement(new T1(i));
			threads.lastElement().start();
		}
		for(int i =0 ; i < NumThreads; i++ ) try{ 
			threads.get(i).join();
			//System.out.println(threads.get(i).v);
		}catch(Exception e){e.printStackTrace();}

		
	}
	
}
