Clock
=====

Weather clock application.

A brief history of my Zipit Z2
Posted on April 7, 2012 by shaun	

For a while now I have been playing with the idea of building my version of the perfect alarm clock. My original ideas involved the Arduino and the Arduino Ethernet Shield. This all changed when I was introduced to the Zipit Z2.

Unlike the Arduino, it comes with an integrated keyboard and chair screen. For further hardware specifications click here.

My investigations of the Z2 led me to Mozzwald’s site which contains most of what I wanted to know. There are Zipit guides on U-Booting, Ubuntu, etc.

While my Zipit was winging its way from the USA, I started work on my clock application. Using the Lazarus IDE and the Free Pascal Compiler I created the first draft. Setting up the cross compile environment was straight forward and I soon had a binary ready for the Zipit’s ARM v5 compatible processor.

After a week of anticipation, I was finally introduced to my Zipit at the Rochester Can’t Hack meet. I started by flashing the Z2 with a U-Boot image I downloaded. To get the image flashed I booted into the Z2′s stock OS and selected the reset to defaults option.

Using Mozzwald’s z2sidX image I booted into Debian Sid. Following this was a back and forth between the Can’t Hack group about keyboard layout. Needless to say I was a very happy to see X start and spent a few minutes playing Free Doom.

For the past few days I have been perfecting the sid image I am using, including changes that allow for better integration of my Clock application.

Some notes on my changes:

Installed avahi-daemon openssh-clients nfs-common

For the dbus install (avahi-deamon dependancy) I selected not to overwrite the dbus config file with the package maintainer’s version. This caused issues with dbus failing to load. To fix this I purged dbus and reinstalled avahi-daemon.

Something I noted was the MAC address used by Debian Sid was not the MAC address printed on a sticker found under the battery. To fix this I edited /etc/network/interfaces.

user@zipit:~$ cat /etc/network/interfaces
# Used by ifup(8) and ifdown(8). See the interfaces(5) manpage or
# /usr/share/doc/ifupdown/examples for more information.
auto wlan0
iface wlan0 inet dhcp
hwaddress ether 00:12:34:56:78:ab

To change the host name to “zipit” I used the hostname command and also edited /etc/hostname and the /etc/hosts file.

Note: I installed avahi-daemon to enable machines on the local network to find the Zipit on the network using the .local domain. i.e. ping zipit.local will resolve. This is why /etc/hosts contains the information below.

127.0.0.1 localhost.localdomain localhost zipit

I then configured samba by editing  /etc/samba/smb.conf and used pdbedit to add a samba user login.
# pdbedit -a -u user

I have noted that the EWOC script provided on the z2sidX image has trouble configuring the wireless network when SSID’s contain spaces. I have fixed this issue and informed Mozzwald.

My work then shifted to my Clock application. I started adding features including a five day weather report, a week day alarm, timer and reminders. Another feature I wanted to add, was the ability to play music from a NFS share. I tested mpg123 and found that mp3′s played well when they were on the local flash drive but streaming off the NFS share was unworkable due to stuttering.

After adding the MP3 playback feature to Clock, I purchased a 32GB Sandisk Micro SDHC card to use for Zipit music storage. To my disappointment the card did not work with the Zipit Z2. The boot precess ended with I/O errors.

It seemed my dreams of adding music to the Zipit were in tatters, but then the word “buffers” popped into my head. When mpg123 plays back MP3′s on the Zipit it only uses about 15% of the processor’s capabilities. The stuttering had to be related to networking because local playback worked, the solution had to be buffering.

I changed the mpg123 command line for Clock to include a 2MB output buffer. Problem solved.

Clock's main window showing five day weather report.

My remaining issues include a long reboot time which I believe is related to the error – libertas failed to find firmware.

On boot up I get the error “boot wlan0 link not ready” from addrconf
and 200 second boot time.

Thanks go to Tristan and the Can’t Hack group, Mozzwald and all those that contributed to z2sid, Lazarus, FPC, Linux, GNU, etc …

