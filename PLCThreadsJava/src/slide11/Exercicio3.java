package slide11;

import java.util.ArrayList;


class Contador3 extends Thread{
	int X, id;
	public Contador3(int X, int id){ this.X = X; this.id = id;}
	public Contador3(){}
	public void run(){
		for (int i = 0; i < X; i++) {
			System.out.println("Thread ("+id+"):  " + i);
		}
	}
}

class GlobalCounter{
	private static int NEXT = 0;
	public static int getNext(){return NEXT++; }
}

class Contador4 extends Thread{
	int id, X;
	public Contador4(){}
	public Contador4(int X, int id){ this.X = X; this.id = id;}
	public void run(){
		int NEXT;
		while(true){
			NEXT = GlobalCounter.getNext();
			if(NEXT >= X) break;
			System.out.println("Thread ("+id+"): " + NEXT);
		}
	}
}


public class Exercicio3 {
	public static void main(String[] args) {
		int N = 3, X = 50;
		ArrayList<Contador3> threadsC3 = new ArrayList<>(); 
		ArrayList<Contador4> threadsC4 = new ArrayList<>(); 
		System.out.println("------- Utilizando contador local -------");
		// Threads utilizando contadores locais
		for(int i = 0 ; i  <N; i++){
			/*
			 * Os resultados se misturam devido ao escalonamento das threads que não pode ser previsto
			 * */			
			threadsC3.add(new Contador3(X,i));
			threadsC3.get(i).start();
		}
		for(int  i = 0 ; i < N; i++) try{ threadsC3.get(i).join(); } catch(InterruptedException e){ e.printStackTrace();} 
		
		
		System.out.println("\n------ Utilizando contador global -------");
		//Threads utilizando um contador globalsd
		for(int i =  0 ; i < N; i++) {
			/*
			 * Além de resultados misturados, existe uma inconsistência nos dados, 
			 * as vezes vemos o mesmo valor sendo impresso várias vezes 
			 */
			threadsC4.add(new Contador4(X,i));
			threadsC4.get(i).start();
		}
		for(int  i = 0 ; i < N; i++) try{ threadsC4.get(i).join(); } catch(InterruptedException e){ e.printStackTrace();} 
				
		
	}
	
	
	
}
