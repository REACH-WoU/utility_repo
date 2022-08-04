# utility_repo
Repo that contain all utility scripts (check_kobo, utils_cleaning, utils_analysis, utils_audit) etc.

## usage instructions

The way to use these utils scripts is to initialize them as a [git submodule](https://git-scm.com/book/en/v2/Git-Tools-Submodules) in your project repo (preferably inside _src_).

### how to switch

Open your shell and move to your _src_ directory:

- ```cd src```

Add the __utility_repo__ submodule:

- ```git submodule add https://github.com/REACH-WoU-Regional/utility_repo.git```

After this you should see two changes to your base repository. Run `git status` to see them: you should have new files named _.gitmodules_ and _utility_repo_. 

Check that everything is okay by using the following command:

- ```git submodule status```

The output should be something like:

``` 
66361b5ef3498cfce3435c680b69e9f5f45d2af1 src/utility_repo (heads/main) 
```

As you can see by default the __utility_repo__ submodule is on branch `main`. Please create your own dev branch to implement your changes to utils:

- ``` cd utility_repo```
- ```git checkout -b dev_DB``` (or provide a different name for your branch)

Now you can add changes to __utility_repo__, for example you can move here your existing utils scripts (if they're not already included in this repo). Change your directory out of utility_repo back to your base repository folder and run `git status` again: you should see something like this:

```
Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git restore <file>..." to discard changes in working directory)
        modified:   utility_repo (new commits)
```

Git saw that you switched to a new branch on the submodule and regards this as a new commit. This is because git is tracking changes inside submodules separately from the base repository - every time you want to make changes to __utility_repo__ you should change directory to `src/utility_repo` and make new commits there, then move back to the base repo and commit changes there.

You will need to update the paths to your util scripts in your _init.R_ (or anywhere else where you're sourcing them):

For example: ```source("./src/utils.R")``` -> change to ```source("./src/utility_repo/utils.R")```

Of course, if your previous _utils.R_ contained some functions that are not included in __utility_repo__, you should move them there.
