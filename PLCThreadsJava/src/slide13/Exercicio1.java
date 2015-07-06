package slide13;

import java.util.Random;
import java.util.Vector;

class Node{
	int val; 
	Node left = null, right = null;
	Node(){}
	Node(int val){ this.val = val;}
}
/*
 * Funcionar com várias threads significa não ocorrer erros de inconsistência de dados devido ao fato
 * de uma thread ter modificado o trabalho realizado por outra thread quando não deveria ocorrer
 * isto
 * */
class BST1{
	Node root = null;
	private Node insert(Node no, int val) {
		if(no == null) return new Node(val);
		if(val > no.val ) no.right = insert(no.right, val);
		else no.left = insert(no.left, val);
		return no;
	}
	public synchronized void insertSynch(int val){
		this.root = insert(root, val);
	}
	public void insertWithoutSynch(int val){
		this.root = insert(root, val);
	}
	public boolean anda(Node no){
		if(no == null) return true;
		boolean  ok = (no.left == null ) || no.left.val <= no.val; 
		ok &= (no.right == null) || no.right.val > no.val;  
		ok &= anda(no.left);
		ok &= anda(no.right);
		return ok;
	}		
}
class T1 extends Thread{
	int qtd = 2000;
	private Random r = new Random();
	public void run(){
		while(qtd-- > 0 ) Exercicio1.tree.insertSynch(r.nextInt(1000));
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
public class Exercicio1 {
	static BST1 tree = new BST1();
	public static void main(String[] args) {
		long curr = System.currentTimeMillis(); 
		int N = 50;
		boolean useThreads = true;
		double s;
		if(useThreads) {
			Vector<T1> v = new Vector<>();
			for(int i =0 ;  i < N; i++) { v.add( new T1() ); v.lastElement().start(); }
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













