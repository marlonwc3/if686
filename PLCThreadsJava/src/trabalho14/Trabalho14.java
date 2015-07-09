package trabalho14;

import java.util.Random;
import java.util.Scanner;
import java.util.Vector;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/*
 * Em todas as implementações do banheiro compartilhado possuimos apenas 1 variável compartilhada, ou  seja
 * apenas uma região crítica, e sincronizamos o acesso à essa região em cada uma das implementações.
 * 
 * */




class BanheiroCompartilhadoWithSync{
	int count =  0; // < 0 -> homem  |   >  0  -> mulher  |  == 0 -> vazio 
	void  entrarHomem(){
		boolean entrou = false; 
		do{ 
			synchronized (this) {
				if(count <= 0 ) { count --; entrou = true; } 
			}
		} while(!entrou);
	}
	void sairHomem(){
		synchronized (this) {
			count++;
		}
	}
	void entrarMulher(){
		boolean entrou = false; 
		do{ 
			synchronized (this) {
				if(count >= 0 ) { count++; entrou = true; } 
			}
		} while(!entrou);
	}
	void sairMulher(){
		synchronized (this) {
			count--; 
		}
	}
}


class BanheiroCompartilhadoWithTravas{
	Lock countLock = new ReentrantLock();
	int count; 
	void  entrarHomem(){
		boolean entrou = false; 
		do{ 
			countLock.lock();
			try{ 
				if(count <= 0 ) { count --; entrou = true; }
			}
			finally { countLock.unlock(); } 
		} while(!entrou);
	}
	void sairHomem(){
		countLock.lock();
		try{
			count++; 
		} finally { countLock.unlock(); }
	}
	void entrarMulher(){
		boolean entrou = false; 
		do{ 
			countLock.lock();
			try{ 
				if(count >= 0 ) { count++ ; entrou = true; }
			}
			finally { countLock.unlock(); } 
		} while(!entrou);
	}
	void sairMulher(){	
		countLock.lock();
		try{
			count--; 
		} finally { countLock.unlock(); }		
	}
}

class BanheiroCompartilhadoWithVariveisCond{
	Lock countLock = new ReentrantLock(true);
	int count;
	Condition countCondition = countLock.newCondition();
	void entrarHomem(int id){
		countLock.lock();
		try{
			while(count > 0 ) {
				try { countCondition.await(); } catch(InterruptedException e) { e.printStackTrace(); }
			}
			System.out.println("Homem ("+id+") entrou !");
			count--; 
			
		}finally{
			countLock.unlock(); 
		}
	}
	void sairHomem(int id) {
		countLock.lock(); 
		try{
			count++; 
			System.out.println("Homem ("+id+") saiu ! count: " + count );
			if( count >= 0 ) {
				countCondition.signalAll();
			}
		}finally{  countLock.unlock(); }	
	}
	void entrarMulher(int id){
		countLock.lock();
		try{
			while(count < 0 ) {
				//System.out.println("OIEEE");
				try { countCondition.await(); } catch(InterruptedException e) { e.printStackTrace(); }
			}
			count++; 		
			System.out.println("Mulher ("+id+") entrou !");
		}finally{
			countLock.unlock(); 
		}
	}
	void sairMulher(int id) {
		countLock.lock(); 
		try{
			count--; 
			if( count <= 0 ) {
				countCondition.signalAll();
			}
			System.out.println("Mulher ("+id+") saiu ! count: " + count );
		}finally{  countLock.unlock(); }	
	}		
}

class Pessoa extends Thread {
	int qtd = Trabalho14.qtd ; 
	int id; 
	boolean sexo; 
	public Pessoa(){ qtd = Trabalho14.qtd; }
	public Pessoa(int _id, boolean _sexo ){ id = _id; sexo = _sexo; }
	public void run(){
		qtd += (qtd%2 ==  1) ? 1:0 ; 
		for(int i =0 ; i < qtd; i++ ){ 
			if(sexo) {
				if(i % 2 == 0 ) Trabalho14.banheiro.entrarHomem(id);
				else Trabalho14.banheiro.sairHomem(id);
			}
			else{
				if(i %2 == 0 ) Trabalho14.banheiro.entrarMulher(id);
				else Trabalho14.banheiro.sairMulher(id);
			}
		}		
	}
}


// Testando com variáveis condicionais 
public class Trabalho14 {
	static BanheiroCompartilhadoWithVariveisCond banheiro =  new BanheiroCompartilhadoWithVariveisCond(); 
	static int qtd=  5; 
	static int N = 5; 
	public static void main(String[] args) {
		Vector<Pessoa> threads = new Vector<>();
		for(int i =0 ; i < N; i++ ) { threads.add( new Pessoa(i, i%2 == 0 )  ); threads.lastElement().start();}
		for(int i =0 ; i < N; i++) { try{ threads.get(i).join();  }catch(Exception e ) {e.printStackTrace();}  }
	}

}
