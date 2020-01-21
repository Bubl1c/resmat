#include "std_lib_facilities.h"
#include <cmath>
#include <stdio.h>
int main()
{
int n; // кількість фігур вводиться ззовні
int m; // положення в просторі - 4 варіанти 0 90 180 270 град- 1 2 3 4 відповідно
int k; // додатня чи відєна фігура 1 або 2 - змінити всі аутпути на мінус
//початок програми
setlocale(LC_ALL, "Ukrainian");
cout<<"кiлькiть елементi перерiзу n=";
 cin>>n;
 vector<double> area(n), I_y(n),I_z(n), I_yz(n),y_c(n),z_c(n);
 double y_center,z_center;
//задається варіантом попередньо
 for(int i=0;i<n;i++){
	cout<<"A"<<i+1<<"=";
cin>>area[i];
cout<<"Iy"<<i+1<<"=";
cin>>I_y[i];
cout<<"Iz"<<i+1<<"=";
cin>>I_z[i];
cout<<"Iyz"<<i+1<<"=";
cin>>I_yz[i];

//кординати фігури
cout<<"yc"<<i+1<<"=";
cin>>y_c[i];
cout<<"zc"<<i+1<<"=";
cin>>z_c[i];
 }
// розрахунок центра ваги
 double r1,A_sum,r3;
 r1=0.;
 A_sum=0.;
 r3=0.;
 for(int i=0;i<n;i++){
	 r1+=area[i]*y_c[i];
	 A_sum+=area[i];
	 y_center=r1/A_sum;

	 r3+=area[i]*z_c[i];
	 z_center=r3/A_sum;
 }
  cout<<y_center<<endl;
  cout<<z_center<<endl;

//y_center z_center координати загального центра ваги

// центральні моменти інерції
 double  I_yc,I_zc, I_yzc;
 I_yc=0.;
 I_zc=0.;
 I_yzc=0.;
 double a(n);
for(int i=0;i<n;i++){
  double a;
  ai = z_c[i]- z_center;
  I_yc+=(I_y[i]+area[i]*pow((z_c[i]- z_center),2));
   I_zc+=(I_z[i]+area[i]*pow((y_c[i]- y_center),2));
   I_yzc+=(I_yz[i]+area[i]*(z_c[i]- z_center)*(y_c[i]- y_center));
   a[i] = ai;
}
 cout<<I_yc<<endl;
  cout<<I_zc<<endl;
  cout<<I_yzc<<endl;

//головна система координат
double alfa,alfa1;
alfa=atan (2.* I_yzc/( I_zc- I_yc))/2.;
alfa1=atan (2.* I_yzc/( I_zc- I_yc)) * 90./3.141592;
cout<<alfa1<<endl;

//головні моменти інерції
double I_u,I_v;
I_u=I_yc*cos(alfa)*cos(alfa)+I_zc*sin(alfa)*sin(alfa)-I_yzc*sin(2.*alfa);
I_v=I_zc*cos(alfa)*cos(alfa)+I_yc*sin(alfa)*sin(alfa)+I_yzc*sin(2.*alfa);
  cout<<I_u<<endl;
  cout<<I_v<<endl;

//Перевірка головних моментів інерції
double I_max,I_min;
I_max=(I_yc+I_zc)/2.+pow((pow((I_yc-I_zc)/2.,2)+pow(I_yzc,2)),0.5);
I_min=(I_yc+I_zc)/2.-pow((pow((I_yc-I_zc)/2.,2)+pow(I_yzc,2)),0.5);
  cout<<I_max<<endl;
  cout<<I_min<<endl;

//радіуси інерції
double i_max,i_min;
i_max=pow(I_max/A_sum, 0.5);
i_min=pow(I_min/A_sum, 0.5);
 cout<<i_max<<endl;
 cout<<i_min<<endl;
keep_window_open();

}