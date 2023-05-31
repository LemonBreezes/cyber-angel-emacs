sudo apt update && sudo apt -y upgrade
sudo apt install -y xfce4 xfce4-goodies
sudo apt install -y xrdp
sudo cp /etc/xrdp/xrdp.ini /etc/xrdp.ini.bak
sudo sed -i 's/3389/3390/g' /etc/xrdp/xrdp.ini
sudo sed -i 's/max_bpp=32/#max_bpp=32\nmax_bpp=128/g' /etc/xrdp/xrdp.ini
sudo sed -i 's/xserverbpp=24/#xserverbpp=24\nxserverbpp=128/g' /etc/xrdp/xrdp.ini
echo xfce4-session > ~/.xsession
sudo systemctl enable dbus
sudo systemctl enable xrdp
sudo /etc/init.d/dbus start
sudo /etc/init.d/xrdp start
