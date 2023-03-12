#To compile Ged2Dendro on your Mac#

Probably works on any Mac that has OpenDarwin as its kernel: basically anything MacOsX. It was tried and found to work on  OSX Catalina. If you have an older version like El Capitan, this will not work.

Use the Terminal. We used the bash shell, which is default. Ghcup likes bash, too.

Required applications: curl, xsltproc, git, ghcup.

Most Mac distributions already have curl and xsltproc installed. 

To install ghcup, use these commands:
```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org > ./ghcup.sh  
sudo chmod +x ./ghcup.sh  
./ghcup.sh  
```  

Running ghcup will install ghc, ghci, and other tools and libraries required by those. Ghcup will try to update itself a few times and will install ghc. Which is big. It takes a while: it has lots of dependencies that need building on your particular machine for the first time.

Once done, ghcup will ask whether you want to source its own environment into your shell. Let it do so. If it complains it can't find a bashrc file in your home directory: that may be OK. There isn't any in mine either. It doesn't seem to hurt anything.

At this stage you want to quit and restart your terminal session. Then test whether cabal and ghc got installed, using these commands: 
```sh
    which cabal  
    which ghc  
```

Both of those should wield a response in the form of a file location. If not, something failed to install.

If it failed, then use this command to have ghcup show a graphical interface with options of tools to install:  
```sh
    ghcup tui  
```

In the list of tools you will see GHC, cabal, HLS, and GHCup, depending on the version of GHCup.

You need to install cabal. The HLS is a nicety for haskell programmers; not necessary for building programs from haskell source code.

Then create a new folder wherever you want it, to receive the ged2dendro source code. Navigate into that folder, and tell git to download the source code using the following command:  
```sh
  git clone https://git.sr.ht/~aev/ged2dendro 
```

On our Mac, cabal refused to install. So we had to jump through some hoops and do a difficult installation. On your computer, if cabal did install, things will be easier.

Navigate into the ged2dendro directory, then type:
```sh
   cabal build  
```

That will download and precompile everything the program needs. It may take a few minutes. It will also tell you whether your computer has the ghc and cabal that are capable of compiling the program, and warn you if you need other versions. If so, you can use ghcup tui to get other versions.

It may also share a few warnings about the source code, like variables defined but not used. It is safe to ignore those.

Once done, use the following command to install the program onto your Mac so you can start using it from the command line:

```sh
    cabal install  
```

Wait for that to finish (should be a few seconds), and confirm that your terminal recognizes the program by typing: 

```sh
    which ged2dendro  
```

which should result in the terminal responding with the path to the program. If so: success!


On our Mac, cabal didn't want to install via ghcup. So we had to copy ghc manually and compile it ourselves. And that was no fun.

But once we got that done, we can build the program using this command:
```sh
  ghc --make -O2 -o ./bin/ged2dendro ./Main.hs  
```

And then we get to do our own symlinking to make the shell know that program. 

After that, we can use the program as described in the README.md.

