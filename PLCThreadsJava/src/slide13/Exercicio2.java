package slide13;

import java.util.Random;
import java.util.Vector;


class BST2{
		Node root = null;

    private Node insert(Node no, int val) {
        synchronized (BST2.class){
                if(no == null) { return new Node(val); }
        }
        synchronized (no ){
                if(val > no.val ) no.right = insert(no.right, val);
        }
        synchronized (no ) {
                if( val <= no.val) no.left  = insert(no.left, val);
        }
        return no;
    }
    public void insert(int val){
    	synchronized (root) {
			root = insert(root, val);
		}
    }
	
	boolean anda(Node no){
		if(no == null) return true;
		boolean  ok = (no.left == null ) || no.left.val <= no.val; 
		ok &= (no.right == null) || no.right.val > no.val;  
		ok &= anda(no.left);
		ok &= anda(no.right);
		return ok;
	}
}


class T2 extends Thread{
	int qtd = 2000;
	private Random r = new Random();
	public void run(){
		while(qtd-- > 0 ) Exercicio2.tree.insert(r.nextInt(1000));
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
public class Exercicio2 {
	static BST2 tree = new BST2();
	public static void main(String[] args) {
		long curr = System.currentTimeMillis(); 
		int N = 50;
		boolean useThreads = true;
		double s;
		if(useThreads) {
			Vector<T2> v = new Vector<>();
			for(int i =0 ;  i < N; i++) { v.add( new T2() ); v.lastElement().start(); }
			for(int i =0; i < N; i++ ) { try {v.get(i).join(); }catch(Exception e) {e.printStackTrace();}   }
		}
		else{
			// 50*2000 == 100000 inserções
			Random r = new Random();
			for(int i =  0; i < 100000; i++){
				Exercicio1.tree.insertWithoutSynch(r.nextInt(1000));
			}
			
		}
		long after = System.currentTimeMillis();
		s = after - curr; s/= 1000.0;
		System.out.println("Tempo: " + s + " segundos"  );
		System.out.println("Anda: " + tree.anda(tree.root) );



	}
}








