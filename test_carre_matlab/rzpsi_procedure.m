clc
clear
close all
%% test for carre rzpsi procedure
    % get the data containing the (r,z,psi) information
[r,z,psi]=read_gfile('sd.equ');

%% get the basic dimension and mid variablles
[nz,nr]=size(r);
r0=r(1,:);
z0=z(:,1);
stp0=min([max([abs(r0(1)-r0(end)),abs(z0(1)-z0(end)),1]),10])/1e3;
%% get the rmax
rmax=0 ;
for i=1:nr-1 % don't display the "："
    rmax=max(rmax,r0(i+1)-r0(i)^2);
end

%% get for zmax
zmax=0 ;
for i=1:nz-1 % don't display the "："
    zmax=max(zmax,z0(i+1)-z0(i)^2);
end

%% eps_xpt
eps_xpt=(rmax+zmax)^0.5;

%% artificaially for DDN configuration
    % don't understand
cstlin=0;
% psi=psi+min(cstlin*())

%% read the structure.dat
stru=read_structure('structure.dat');

%% get the derive

[psidx,psidy]=gradient(psi,abs(r0(2)-r0(1)),(z0(2)-z0(1)));
% figure
% pcolor(r,z,psidx);shading interp; colorbar;axis equal
% caxis([0 0.01])
% colormap(hsv)

%% plot the level lines for psidx=0 and psidy=0 
    % plot the line which dpsi/d=0
tmp=psidx;
% tmp(tmp>eps)=nan;
figure
cmat=contourc(r0,z0,tmp,[eps,eps]);%shading interp;axis equal;
cn_psidx=cmat2node(cmat);
contour(r,z,tmp,[eps,eps]);%shading interp;axis equal;
tmp1=psidy;
% tmp1(tmp1>eps)=nan;
hold on
cmat=contourc(r0,z0,tmp,[eps,eps]);%shading interp;axis equal;
cn_psidy=cmat2node(cmat);
contour(r,z,tmp1,[eps,eps]);axis equal;

%% test for insection
[x0,y0,iout,jout]=intersections(cn_psidx(:,1),cn_psidx(:,2),cn_psidy(:,1),cn_psidy(:,2));
iout(isnan(iout))=[];
jout(isnan(jout))=[];




hold on
plot(x0(iout),y0(jout),'cd')

%% calculate the XY plane coefficent
    % f(x,y)=a00+a10*x+a01*y+a11xy
    % step 1:deltxy
z0=z0';
% deltxy=(r(:,2:end)-r(:,1:end-1)).*(z(2:end,:)-z(1:end-1,:));

%% (i,j)=f(i,j)*x(i+1)*y(j+1) ...f(i+1,j)*x(i)*y(j+1)
%% xy and (xy)^0 => [1 -1 -1 1]; x or y => [-1 1 1 -1]

for j=1:length(z0)-1
   for i=1:length(r0)-1
       tmp=(r0(i+1)-r0(i))*(z0(j+1)*z0(j));
%        deltxy(i,j)=tmp;
       a00(i,j)=(psi(i,j)*r0(i+1)*z0(j+1)-psi(i+1,j)*r0(i)...
           *z0(j+1)-psi(i,j+1)*r0(i+1)*z0(j)+psi(i+1,j+1)*...
           r0(i)*z0(j))/tmp; 
       a10(i,j)=(-psi(i,j)*z0(j+1)-psi(i+1,j)...
           *z0(j+1)-psi(i,j+1)*z0(j)+psi(i+1,j+1)*...
           z0(j))/tmp; 
       a01(i,j)=(psi(i,j)*r0(i+1)-psi(i+1,j)*r0(i)...
           -psi(i,j+1)*r0(i+1)+psi(i+1,j+1)*...
           r0(i))/tmp; 
       a11(i,j)=(psi(i,j)-psi(i+1,j)-psi(i,j+1)+psi(i+1,j+1))/tmp; 
   end
end
coeff_a00(:,:,1)=a00;
coeff_a10(:,:,1)=a10;
coeff_a01(:,:,1)=a01;
coeff_a11(:,:,1)=a11;

%% for psidx
psi=psidx;
for i=j:length(z0)-1
   for i=1:length(r0)-1
       tmp=(r0(i+1)-r0(i))*(z0(j+1)*z0(j));
%        deltxy(i,j)=tmp;
       a00(i,j)=(psi(i,j)*r0(i+1)*z0(j+1)-psi(i+1,j)*r0(i)...
           *z0(j+1)-psi(i,j+1)*r0(i+1)*z0(j)+psi(i+1,j+1)*...
           r0(i)*z0(j))/tmp; 
       a10(i,j)=(-psi(i,j)*z0(j+1)-psi(i+1,j)...
           *z0(j+1)-psi(i,j+1)*z0(j)+psi(i+1,j+1)*...
           z0(j))/tmp; 
       a01(i,j)=(psi(i,j)*r0(i+1)-psi(i+1,j)*r0(i)...
           -psi(i,j+1)*r0(i+1)+psi(i+1,j+1)*...
           r0(i))/tmp; 
       a11(i,j)=(psi(i,j)-psi(i+1,j)-psi(i,j+1)+psi(i+1,j+1))/tmp; 
   end
end
coeff_a00(:,:,2)=a00;
coeff_a10(:,:,2)=a10;
coeff_a01(:,:,2)=a01;
coeff_a11(:,:,2)=a11;
%% for psidx
psi=psidy;
for j=1:length(z0)-1
   for i=1:length(r0)-1
       tmp=(r0(i+1)-r0(i))*(z0(j+1)*z0(j));
%        deltxy(i,j)=tmp;
       a00(i,j)=(psi(i,j)*r0(i+1)*z0(j+1)-psi(i+1,j)*r0(i)...
           *z0(j+1)-psi(i,j+1)*r0(i+1)*z0(j)+psi(i+1,j+1)*...
           r0(i)*z0(j))/tmp; 
       a10(i,j)=(-psi(i,j)*z0(j+1)-psi(i+1,j)...
           *z0(j+1)-psi(i,j+1)*z0(j)+psi(i+1,j+1)*...
           z0(j))/tmp; 
       a01(i,j)=(psi(i,j)*r0(i+1)-psi(i+1,j)*r0(i)...
           -psi(i,j+1)*r0(i+1)+psi(i+1,j+1)*...
           r0(i))/tmp; 
       a11(i,j)=(psi(i,j)-psi(i+1,j)-psi(i,j+1)+psi(i+1,j+1))/tmp; 
   end
end
coeff_a00(:,:,3)=a00;
coeff_a10(:,:,3)=a10;
coeff_a01(:,:,3)=a01;
coeff_a11(:,:,3)=a11;

%% step 6: determine the point where the derivatives in x and y
    % symbol function
% syms x00 x10 x01 x11 y00 y10 y01 y11 vx vy
% px=x00+x10*vx+x01*vy+x11*vx*vy;
% py=y00+y10*vx+y01*vy+y11*vx*vy;
% 
% [vx,vy]=solve(px,py,vx,vy);
% [vx;vy]

% 通过消元法计算跟
npxtot=0;
for j=1:length(z0)-1
    for i=1:length(r0)-1
        a=(coeff_a01(i,j,3)*coeff_a11(i,j,2)-coeff_a11(i,j,3)...
            *coeff_a01(i,j,2));
        b=(coeff_a00(i,j,3)*coeff_a11(i,j,2)-coeff_a10(i,j,3)...
            *coeff_a01(i,j,2)...
            +coeff_a01(i,j,3)*coeff_a10(i,j,2)-coeff_a11(i,j,3)...
            *coeff_a00(i,j,2));
        c=(coeff_a00(i,j,3)*coeff_a10(i,j,2)-coeff_a10(i,j,3)...
            *coeff_a00(i,j,2));
        %% check
        if(4*a*c<b^2)
            
            if(b>0 & a~=0)
                y1=-2*c/((b^2-4*a*c)^0.5+b);
                y2=(-b-(b^2-4*a*c))/(2*a);
            elseif(a~=0)
                y1=2*c/((b^2-4*a*c)^0.5-b);
                y2=(-b+(b^2-4*a*c))/(2*a);
            elseif(b~=0)
                y1=-c/b;
            else
                y1=2*z0(1)-z0(2);
            end
            
            %% test to see if the edge contains one of these roots
            if (y1>=z0(j) & y1<=z0(j+1))
                if(coeff_a10(i,j,2)+coeff_a11(i,j,2)*y1~=0)
                    x1=(-coeff_a00(i,j,2)-coeff_a01(i,j,2)*y1)/...
                        (coeff_a10(i,j,2)+coeff_a11(i,j,2)*y1);
                else
                    x1=2*r0(1)-z0(2);
                end
                
                
                %% save each point where the gradient is canceled.
                if(x1>=r0(i) & x1<=r0(i+1))
                    npxtot=npxtot+1;
                    pointx(npxtot)=x1;
                    pointy(npxtpt)=y1;
                    ii(npxtot)=i;
                    jj(npxtot)=j;
                end        
            end
            
            if (a~=0)
                if(y2>=z0(j) & y2<=z0(j+1))
                    x2=(-coeff_a00(i,j,2)-(a01(i,j,2)*y2))/...
                        (coeff_a10(i,j,2)+(coeff_a11(i,j,2)*y2));
                else
                    x2=2*r0(1)-r0(2);
                end
                if (x2>=r0(i) &x2<=x(i+1))
                    npxtot=npxtot+1;
                    pointx(npxtot)=x2;
                    pointy(npxtpt)=y2;
                    ii(npxtot)=i;
                    jj(npxtot)=j;
                end
            end
            
        end
        
        
        
    end
end

%% test for intepolation
figure
px=coeff_a00(:,:,1)+coeff_a10(:,:,1).*r(1:end-1,1:end-1)...
    +coeff_a01(:,:,1).*z(1:end-1,1:end-1)+coeff_a11(:,:,1)...
    .*r(1:end-1,1:end-1).*z(1:end-1,1:end-1);
pcolor(px);shading interp;



