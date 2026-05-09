%% spectral flux under and above 200 Hz
%fn= '/Users/tuomaseerola/Downloads/IndianRhythmCycleAlignment-main/audio/Teental_Stm1_5.mp3';
%fn= '/Users/tuomaseerola/Downloads/IndianRhythmCycleAlignment-main/audio/Rupak_Stm1_5.mp3';
fn= '/Users/tuomaseerola/Downloads/IndianRhythmCycleAlignment-main/audio/Jhaptal_Stm1_5.mp3';
%fn= '/Users/tuomaseerola/Downloads/IndianRhythmCycleAlignment-main/audio/Keherva_Stm1_5.mp3';
a = miraudio(fn, 'Extract',0, 10);
%
afl = mirfilterbank(a,'Manual',[-Inf 200]);
afh = mirfilterbank(a,'Manual',[200 Inf]);
%sp= mirspectrum(afl);
%sp= mirspectrum(afh)
%axis([0 1000 0 Inf])
%
%figure(1)
f1 = mirflux(mirframe(afl,'Length', 0.05,'s','Hop',1));
f2 = mirflux(mirframe(afh,'Length', 0.05,'s','Hop',1));
% export
f11 = mirgetdata(f1);
f22 = mirgetdata(f2);
t=linspace(0,(199*0.05+0.05),length(f11));
%writematrix([t;f11]','Keherva_flux_low.csv');
%writematrix([t;f22]','Keherva_flux_high.csv');
disp("done")

%% pick peaks
p=mirpeaks(f1);
[x,y]=mirgetdata(p);
tmp = [x y];
tmps=sort(tmp,1);
writematrix(tmps,'Jhaptal_flux_low_peaks.csv');

p2=mirpeaks(f2);
[x2,y2]=mirgetdata(p2);
tmp = [x2 y2];
tmps=sort(tmp,1);
writematrix(tmps,'Jhaptal_flux_high_peaks.csv');
disp('peaks done')
