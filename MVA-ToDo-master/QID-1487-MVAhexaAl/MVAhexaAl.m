%% clear all variables
clear all
close all
clc

%% create blue and black points
a    = [-sqrt(3),0,sqrt(3)]';
blue = [-2,-2,-2,-1,-1,-1,0,0,0,1,1,1,2,2,2]';
b    =  [-sqrt(3)/2, sqrt(3)/2, 3*sqrt(3)/2]';

black   =  [-1.5,-1.5,-1.5,-0.5,-0.5,-0.5,0.5,0.5,0.5,1.5,1.5,1.5,2.5,2.5,2.5]';
point1  =  repmat(a,length(blue)/length(a),1);
point2  =  repmat(b,length(black)/length(b),1);
red     = [-0.2,0.2]';

co1 =  sqrt(3)/2-1/sqrt(3);
co2 =  sqrt(3)/2-1/(2*sqrt(3));
co3 =  sqrt(3)/2+1/(2*sqrt(3));
co4 =  sqrt(3)/2+1/sqrt(3);
co5 =  -1/sqrt(3);
co6 =  -1/(2*sqrt(3)) ;

%% plot
figure(1)
hold on
box on
plot(blue, point1,'b.')
xlabel('X')
ylabel('Y') 
xlim([-3,3]),ylim([-3,3])
plot(black, point2,'k.')
plot(-0.2,0.2,'r.') % create a red point
plot([-1,-1,0,0,-1],[0,sqrt(3),sqrt(3),0,0],'b-','LineWidth',2) % creat blue rectangle
plot([-0.5,-0.5,0.5,0.5,-0.5],[-sqrt(3)/2,sqrt(3)/2,sqrt(3)/2,-sqrt(3)/2,-sqrt(3)/2],'k-','LineWidth',2) % creat black rectangle
plot([-0.5,-1,-1,-0.5,0,0,-0.5],[co1,co2,co3,co4,co3,co2,co1],'g-','LineWidth',2)% creat green hexagon
plot([0,-0.5,-0.5,0,0.5,0.5,0],[co5,co6,co1,co2,co1,co6,co5],'y-','LineWidth',2) % creat yellow hexagon
hold off
