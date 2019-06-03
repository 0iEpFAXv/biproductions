cd /tmp/biproductions
mv /home/*.txt .
stack exec biproductions-exe >> /home/stdoe/analytic.log 2>> /home/stdoe/analytic.err
mv *.txt /home/
sleep 1
