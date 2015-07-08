package slide13;

import java.util.Random;
import java.util.Vector;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class Node2{
	public int val; 
	public Node2 left, right;
	public Lock lockLeft, lockRight; 
	Node2(int val){
		this.val = val; 
		this.left = this.right = null;
		this.lockLeft = new ReentrantLock();
		this.lockRight = new ReentrantLock();
	}
}


class BST2{
	Node2 root = new Node2(Integer.MAX_VALUE-5);

    public void insert(Node2 no, int val){  	
    	
    	if(val > no.val ) {
    		if(no.right == null  ) {
        		no.lockRight.lock();
        		try {
        			if(no.right != null) insert(no.right, val);
        			no.right = new Node2(val);
        		}
        		finally {
        			no.lockRight.unlock();
        		}    			
    		}
    		else insert(no.right, val); 
    	}
    	else { 
    		if(no.left == null  ) {
        		no.lockLeft.lock();
        		try {
        			if(no.left != null) insert(no.left, val);
        			no.left = new Node2(val);
        		}
        		finally {
        			no.lockLeft.unlock();
        		}    			
    		}
    		else insert(no.left, val); 
    	}    	
    	
    	
    }
	
		
    public void insert(int val){
    	this.insert(this.root, val);
    }

}


class T2 extends Thread{
	int l, r;
	public T2(int _l , int _r) { l = _l;  r=_r; }
	
	public void run(){
		int x;
		//System.out.println("De " +l + " ate " + r );
		for(int i = l ; i < r; i++ ) {
			x = Exercicio2.v.get(i);
		//	Exercicio2.bst1.insertSynch(x);
			Exercicio2.bst2.insert(x);
			
		}
	//	while(qtd-- > 0 ) Exercicio2.tree.insert(r.nextInt(1000));
	}
}


/*
 * Com threads e toda árvore synchronized: 
 * 	0.213
 *  0.210
 *  0.225
 *  
 * Sem threads:
 * 0.16
 * 0.13
 * 0.18
 * */
public class Exercicio2{
	static Vector<Integer> v = new Vector<>();
	static BST1 bst1 = new BST1();
	static BST2 bst2 = new BST2();
	
	public static void main(String[] args) {
		long l1 = System.currentTimeMillis(); 
		
		Random r = new Random();
		Vector<T2> threads = new Vector<>();
		int N =  400;
		int TOTAL = 200000, L = 0, TAXA; TAXA = TOTAL/N; 
		for(int i = 0 ; i < TOTAL; i++) v.addElement(r.nextInt(1000000));
		for(int i = 0; i < N; i++ ) {
			threads.addElement(new T2(L, (i==N-1) ? (TOTAL) : (L + TAXA)  ));
			L+=TAXA;
			threads.lastElement().start();
		}
		for(int i  =0; i <N; i++ ) try {
			threads.get(i).join();
		} catch (Exception e) {
			// TODO: handle exception
		}
		
		
		System.out.println("Tempo gasto: " + ((System.currentTimeMillis()) - l1 )/1000.0  );
	}
}






