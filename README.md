# cryptoTaxes
Compute your crypto taxes.

## Usage
[java Runtime Environment](https://www.java.com/download/) must be previously installed in your computer in order to run this program. 

Download compressed file (.zip extension for Windows or .tar.gz extension for Linux) from [Releases](https://github.com/cryptoTaxes/cryptoTaxes/releases), uncompress downloaded file and open a terminal in uncompressed folder.

Trading history files should be placed in `data/usr/yourUser/input` folders (where `yourUser` is the folder storing your user data). A `demo` user is provided along with his demo history files, so that you can copy the folder structure for new users.

In order to run the program, type command `run.bat` in Windows terminal or command `./run.sh` in Linux in terminal.

The following command line parameters can be written after the command:

* `-user=yourUser`       to run the program for user `yourUser` (whose history must be in `data/usr/yourUser/input` folder).

* `-verbosity=level`     where level is a number from 0 on, the higher the number, the more information is shown in report.

* `-currency=curr`       where `curr` can be `euro`, `btc` or `usd`.

* `-download-prices=yes` in order to update prices from [Coinmarketcap](coinmarketcap.com) and [BDE](www.bde.es).

After runnng the program, output report will be produced in folder `data/usr/yourUser/output`.

## Configuration

The program can be further configured by editing text files in folder `data/config`.



