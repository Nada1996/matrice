#include <iostream>
#include <string>
#include <cstdlib>
#include <math.h>
#include <sstream>
using namespace std;
string out(float a)
{ ostringstream ss;
   ss << a;
     string  F = ss.str();
  
	 return F;
}
void coutmat (float cmat[][100], int r,int c)
{
	int i,o,sp=c-1;
cout<<"["; 

for(i=0;i<r-1;i++)
{ for(o=0;o<c-1;o++)
    {cout<<out(cmat[i][o]);
		cout<<" ";
    }
   cout<<out(cmat[i][sp]);
   cout<<";";
}
   
   for(o=0;o<c-1;o++)
    { cout<<out(cmat[r-1][o]);
		cout<<" ";
    }
    cout<<out(cmat[r-1][sp]);




cout<<"]";
}
void getmatrix(string s , float Matrix[][100], int &r , int &c)
{
    string u1,D[100];  getline(cin,s);
 int x=s.length() ; u1=s.substr(1,x-2); int i,k=0,o,p=0;float j[100];

for( int i=0; i<u1.length(); i++ ) 
 {if(u1.substr(i,1)==";")
 {k++;
}
 }

for( int i=0; i<u1.length(); i++ ) 
 {if(u1.substr(i,1)==" ")
      {p++;
      }
 }
 r = k+1 ; int sp=(p/r) ;  c = sp +1;
  if (p%r != 0 )
  {cout<<"ERROR";
  exit(0);
  }
 float H[10000] ; H[0]=u1.find(" ",0);
 for(i=1;i<p;i++)
{H[i]=u1.find(" ",(H[i-1]+1));
}
if(k==0)
{r=1;
D[0]=u1;float t = D[0].length();
Matrix[0][0]= atof(D[0].substr(0,H[0]).c_str());

Matrix[0][sp]=atof(D[0].substr((H[sp-1]+1),(t-H[sp-1])).c_str());
for(i=1;i<sp;i++)
{Matrix[0][i]=atof(D[0].substr((H[i-1]+1),(H[i]-H[i-1])).c_str());
}

}
else
{float t = D[0].length(); 
  j[0]=u1.find(";",0);
    for(i=1;i<k;i++)
      {j[i]=u1.find(";",(j[i-1]+1));
	  }
D[0]=u1.substr(0,j[0]);
   for(i=1;i<(k+1);i++)
      {D[i]=u1.substr((j[i-1]+1),((j[i]-1)-j[i-1]));
       } 

  
  
Matrix[0][0]= atof(D[0].substr(0,H[0]).c_str());

Matrix[0][sp]=atof(D[0].substr((H[sp-1]+1),(t-H[sp-1])).c_str());


for(i=1;i<sp;i++)
{Matrix[0][i]=atof(D[0].substr((H[i-1]+1),(H[i]-H[i-1])).c_str());
}


for(i=1;i<r;i++)
{
string z = u1.substr(0,j[i-1]) ; float R = z.length(); float x1= H[sp-1+i*sp];
 float l = D[i].length()-1;

Matrix[i][sp]= atof(D[i].substr(x1-R,l-x1).c_str());
}
 


for(i=1;i<r;i++)
{ string z = u1.substr(0,j[i-1]) ; float R = z.length();
    Matrix[i][0]=atof(D[i].substr(0,(H[i*sp]-R)).c_str()); 
   
}
 
for(i=1;i<r;i++)
          { string z = u1.substr(0,j[i-1]) ; float R = z.length();
     for(o=1;o<sp;o++)
           { Matrix[i][o]= atof(D[i].substr(H[o-1+i*sp]-R,H[o+i*sp]-(H[o-1+i*sp]+1)).c_str());
    
	         }
          }

       }
}
void Transpose (float a[][100],float cmat[][100],int r,int c)
{int i,o;

    for(i=0;i<c;i++)
            {   for(o=0;o<r;o++)
                { cmat[i][o]=a[o][i];
	             }
	   
            } 

}
float getdeterminant(float a[][100],int r)
{float c[100][100];int i,j,k;float deter=0;
    if(r==2)
	{deter=a[0][0]*a[1][1]-a[1][0]*a[0][1];
     return deter;
	}
	else
	{ for(int j=0;j<r;j++)
	  { for(int i=1;i<r;i++)
	      { int h=0;
		  for(k=0;k<r;k++)
	         { if(k!=j)
			    {c[i-1][h]=a[i][k];
			       h++;
				 } 
		     }
		  } 
		  	deter+=pow(-1.0,j)*a[0][j]*getdeterminant(c,r-1);
      }
		
	
	} return deter;
}

void Power(float a[][100],float cmat[][100],int r,int c,float p)
{   int i,o,v,f; float b[100][100];
for(i=0;i<r;i++)
  {for(o=0;o<c;o++)
    { b[i][o]=a[i][o];
	}
  }

	if(c==r&& p>1 && (floor(p) ==p))
{  
   
	for(f=1;f<p;f++)
    {for(i=0;i<r;i++) 
       {
	   for(o=0;o<r;o++)
       {   
             float val=0;
	          for(v=0;v<r;v++)
                {  
             val=val+a[i][v]*b[v][o]; cmat[i][o]=val;
                  }
	          
	   
          }  

       }  
	for(i=0;i<r;i++)
    {     for(o=0;o<c;o++)
        { b[i][o]=cmat[i][o];
	    }
    }
	
	}
	coutmat(cmat,r,r);

	

}
else
{cout<<"ERROR"; exit(0);
}
}
void cofactor(float a[][100],float Inverse[][100],int r,int c)
{ int i,j,k,l;float b[100][100],cmat[100][100];float nada=getdeterminant(a,r);
if(nada==0) {cout<<"ERROR"; exit(0);}
else
 { if(r==c)
  {for(i=0;i<r;i++)
     { for(j=0;j<r;j++)
       {int N=0,A=0;
		   for(k=0;k<r;k++)
         { for(l=0;l<r;l++)
            { 
				if(k!=i && l!=j)
              {b[N][A]=a[k][l];
				if(A<(r-2)) A++;
				else{A=0;N++;}
			  }
				
		    }
		 } cmat[i][j]=pow(-1.0,(i+j))*(1/nada)*getdeterminant(b,r-1); 
     }
	} Transpose(cmat,Inverse,r,c); 
  }
  else{cout<<"ERROR";exit(0); }
 }
}
  
int main ()
{string s,s1;float a[100][100],b[100][100],cmat[100][100];
   int r ,c,r1,c1,i,o;char op;float val;

 getmatrix(s,a,r,c); int sp=c-1;
 
cin>>op;
cin.ignore();
switch (op)
{ case '+':
getmatrix(s1,b,r1,c1);
if(r==r1 && c==c1)
{	for(i=0;i<r;i++)
  {   for(o=0;o<c;o++)
       { cmat[i][o]=a[i][o]+b[i][o];
	   }
	   o=o-c+1;
  } coutmat(cmat,r1,c1);
}
else
{cout<<"ERROR"; exit (0);
}

break;



case '-' :
getmatrix(s1,b,r1,c1);
if(r==r1 && c==c1)
{	for(i=0;i<r;i++)
  {   for(o=0;o<c;o++)
       { cmat[i][o]=a[i][o]-b[i][o];
	   }
	   o=o-c+1;
  } coutmat(cmat,r1,c1);
}
else
{cout<<"ERROR"; exit(0);
}
break;
case '*':
getmatrix(s1,b,r1,c1);
	if(c==r1)
{for(i=0;i<r;i++)
    
  {   for(o=0;o<c1;o++)
       {cmat[i][o]=0;
	   for(int v=0;v<c;v++)
          {  
             cmat[i][o]=cmat[i][o]+a[i][v]*b[v][o]; 
            }
	   }
	   
   }  
 coutmat(cmat,r,c1);  
}
else
{cout<<"ERROR"; exit(0);
}
break;
case '^' :
float p;
cin>>p;
if(r==c)
{	if(p==0)
  { for(i=0;i<r;i++)
   {  for(o=0;o<c;o++)
         { if(i==o) cmat[i][o]=1;
	       else {cmat[i][o]=0;}
          }
     } coutmat(cmat,r,r);
    }
   
   else if(p==1)
{ for(i=0;i<r;i++)
   {  for(o=0;o<c;o++)
         { cmat[i][o]=a[i][o];
          }
     } coutmat(cmat,r,r);
}

   else
   {Power(a,cmat,r,c,p);
   }



}
else{cout<<"ERROR";exit (0);}
break;
case 'T':
	Transpose (a,cmat,r,c);
	coutmat(cmat,c,r);
	break;

case 'D' :
if(r==c){cout<<getdeterminant (a,r);}
	else {cout<<"ERROR"; exit (0); }
	break;

case 'I': 

cofactor(a,cmat,r,c);coutmat(cmat,r,c);
    break;

case '/' :
getmatrix(s1,b,r1,c1);
	if(c==r1)
{float div [100][100];
    cofactor(b,cmat,r1,c1);
    for(i=0;i<r;i++)
    
  {   for(o=0;o<c1;o++)
       {div[i][o]=0;
	   for(int v=0;v<c;v++)
          {  
             div[i][o]=div[i][o]+a[i][v]*cmat[v][o]; 
            }
	   }
	   
   }  
 coutmat(div,r,c1);  
}
else
{cout<<"ERROR"; exit(0);
}
}


    
    
    
    
    
}