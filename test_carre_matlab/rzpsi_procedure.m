clc
clear
close all
%% test for carre rzpsi procedure
    % get the data containing the (r,z,psi) information
[r,z,psi]=read_gfile('sf.equ');

%% get the basic dimension and mid variablles
[nz,nr]=size(r);
r0=r(1,:);
z0=z(:,1);
stp0=min([max([abs(r0(1)-r0(end)),abs(z0(1)-z0(end)),1]),10])/1e3;
%% get the rmax
rmax=0 ;
for i=1:nr-1 % don't display the "£º"
    rmax=max(rmax,r0(i+1)-r0(i)^2);
end

%% get for zmax
zmax=0 ;
for i=1:nz-1 % don't display the "£º"
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
contour(r,z,tmp,[eps,eps]);%shading interp;axis equal;

tmp1=psidy;
% tmp1(tmp1>eps)=nan;
hold on
contour(r,z,tmp1,[eps,eps]);axis equal;










