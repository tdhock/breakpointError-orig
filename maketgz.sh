cd ..
rm -rf breakpointError-JRepStat
cp -r breakpointError-orig breakpointError-JRepStat
rm -rf breakpointError-JRepStat/.git
cd breakpointError-JRepStat
bash figures-remove.sh
cd ..
tar czf breakpointError-JRepStat.tgz breakpointError-JRepStat
du -ms breakpointError-JRepStat*
