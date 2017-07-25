
load("originalValues.RData")

newmath.menu.prompt <- "to return to menu, type me()\n"

current <-
        function (hour = FALSE)
        {
                year = as.integer(substring(Sys.time(), 1, 4))
                month = as.integer(substring(Sys.time(), 6, 7))
                day = as.integer(substring(Sys.time(), 9, 10))
                hr = as.integer(substring(Sys.time(), 12, 13))
                minute = as.integer(substring(Sys.time(), 15, 16))
                second = as.integer(substring(Sys.time(), 18, 19))
                now = c(year = year, month = month, day = day, hour = hr,
                        minute = minute, second = second)
                if (hour)
                        now = c(1, (1/60), (1/3600)) %*% as.numeric(now[c("hour",
                                                                          "minute", "second")])
                now
        }
current.day <-
        function ()
        {
                as.integer(substring(Sys.time(), 9, 10))
        }
current.month <-
        function ()
        {
                as.integer(substring(Sys.time(), 6, 7))
        }
current.year <-
        function ()
        {
                as.integer(substring(Sys.time(), 1, 4))
        }
instructions <-
        function ()
        {# how to use the work functions
                #
                # 1. Run the function work.start -- like this:
                #
                #       work.start()
                #
                #    when you start working. You may add the task, e.g.,
                #
                #       work.start("email")
                #
                # 2. Run work.stop when you stop working. Like this:
                #
                #       work.stop()
                #
                # 3. Work.plot produces the percentile plot.
                #
                # 4. You must edit save.ws so that it saves the workspace where you
                #    want it. You can exit R (without saving the workspace) whenever
                #    you want. Your data will be stored in the saved workspace. The
                #    workspace is saved whenever you add data to the database.
                #
                # 5. Several other functions (work.task, work.duration, etc.) do other
                #    useful things. Print a function to see what it does and how to use it
                #
                # 6. The data is stored in the data frame work. Eventually you will want
                #    to edit work to include only the data you collect.
                #
                # 7. If you fail to run work.stop when you stop working, the next time
                #    you run work.start you will get an error message:
                #
                #      need to fill in most recent duration
                #
                #    You can fix the error (that is, fill in the most recent duration)
                #    with work.duration, giving the duration of the previous work bout.
                #    For example, if the previous work bout lasted 20 minutes, you write:
                #
                #      work.duration(20)
                #
        }
me <-
        function () {
          if (Sys.info()["sysname"]=="Windows"){
            windows()
          } else x11()
          
          menu.choices <- c("Begin warmup session",
                            "Begin testing session",
                            "Show instructions",
                            "Show plots",
                            "Export all trial data to CSV file",
                            "Export only valid trial data to CSV file",
                            "Exit to R command prompt",
                            "Exit to Windows")
          
          switch(menu(menu.choices, graphics = TRUE, title = "Brain Tracking Program"),
                 run.warmup(),
                 run.test(),
                 print(newmath.instructions),
                 see.results(),
                 write.csv(newmath5, row.names=FALSE, file="brain_data_raw.csv"),
                 write.csv(newmath5[which(newmath5$correct==TRUE & newmath5$include==TRUE),], row.names=FALSE, file="brain_data_only_valid.csv"),
                 print("to return to menu, type me()"),
                 quit(save = "no"))
        }
newmath <-
        function ()
        {
        }
newmath.add <-
        function (data = newmath5, da.params = newmath5.analysis, trials = 32,
                  note = "", measure.travel = FALSE, label = "")
        {
                t = list(trial = 1, total.trials = trials, note = note, status = "okay",
                         correct = TRUE, correction.trial = FALSE)
                t$condition = newmath.name.session(data = data, label = label)
                data.analysis <<- da.params
                dataset <<- tail(data, 1)
                data.analysis["start.time"] <<- as.character(Sys.time())
                data.analysis$latency.ptiles.so.far <<- NULL
                if (measure.travel) {
                        data.analysis$travel.ptiles.so.far <<- NULL
                        data.analysis$wait.ptiles.so.far <<- NULL
                }
                while (t$trial <= trials) {
                        t$note = note
                        t = newmath.start.trial(t)
                        newmath.foreperiod(t)
                        t = newmath.choose.problem(t)
                        newmath.show.problem(t)
                        t = newmath.wait.for.answer(t, measure.travel)
                        t = newmath.evaluate.answer(t, measure.travel)
                        newmath.feedback(t)
                        t = newmath.record.trial(t, measure.travel)
                        if (t$status == "end session")
                                break
                        if (t$status == "okay") {
                                if (t$correct) {
                                        if ((!t$correction.trial) | (!data.analysis$correction))
                                                t$trial = t$trial + 1
                                        t$correction.trial = FALSE
                                }
                                else t$correction.trial = TRUE
                        }
                }
                invisible(newmath.end.session(t, old.data = data, measure.travel))
        }
newmath.analyze <-
        function (data = newmath5, n = 400, measure.travel = FALSE)
        {
                cat("updating data and data.analysis\n")
                data.analysis$last.update <<- Sys.time()
                ok = data$include & data$correct
                newmath.set.comparisons(data[ok, ], measure.travel)
                pr = data$problem[ok]
                data$latency.ptile[ok] = newmath.analyze.ptiles(pr, data$latency.msec[ok],
                                                                data.analysis$latency.ptile.comparison)
                if (measure.travel) {
                        data$wait.ptile[ok] = newmath.analyze.ptiles(pr, data$wait.msec[ok],
                                                                     data.analysis$wait.ptile.comparison)
                        data$travel.ptile[ok] = newmath.analyze.ptiles(pr, data$travel.msec[ok],
                                                                       data.analysis$travel.ptile.comparison)
                }
                cat("finished updating data and data.analysis\n")
                invisible(data)
        }
newmath.analyze.comparisons <-
        function (val = newmath5$latency.msec[newmath5$include & newmath5$correct],
                  n = 400, pr = newmath5$problem[newmath5$include & newmath5$correct])
        {
                cn = unique(pr)
                lengths = NULL
                for (nam in cn) lengths = c(lengths, length(val[pr == nam]))
                n = min(n, lengths)
                all = matrix(0, ncol = length(cn), nrow = n, dimnames = list(NULL,
                                                                             cn))
                for (nam in cn) all[, as.character(nam)] = tail(val[pr ==
                                                                            nam], n)
                all
        }
newmath.analyze.distribution <-
        function (data = newmath3)
        {
                rt = data$latency.msec[data.analysis$okay] - data.analysis$subtract.msec
                pwr = data.analysis$power
                if (pwr > 0)
                        tr = rt^pwr
                if (pwr == 0)
                        tr = log(rt)
                if (pwr < 0)
                        tr = -(rt^pwr)
                tr - data.analysis$problem[data$problem[data.analysis$okay]]
        }
newmath.analyze.problem <-
        function (data = newmath3)
        {
                tr = data$latency.msec[data.analysis$okay] - data.analysis$subtract.msec
                pwr = data.analysis$power
                if (pwr > 0)
                        tr = tr^pwr
                if (pwr == 0)
                        tr = log(tr)
                if (pwr < 0)
                        tr = -(tr^pwr)
                a = split(tr, data$problem[data.analysis$okay])
                m = sapply(a, FUN = mean)
                m - mean(m)
        }
newmath.analyze.ptile.comparisons <-
        function ()
        {
        }
newmath.analyze.ptiles <-
        function (problem, value, comparison)
        {
                n = length(problem)
                ptiles = vector(mode = "numeric", length = n)
                for (pr in 1:n) ptiles[pr] = newmath.ptile(problem[pr], value[pr],
                                                           comparison)
                ptiles
        }
newmath.average <-
        function (which = tail(newmath5$condition[newmath5$include],
                               1), data = newmath5, msecf = FALSE, travelf = 0)
        {
                if (10 == nchar(which)) {
                        avs = sapply(newmath.conds(which), FUN = newmath.average,
                                     data = data, msecf = msecf, travelf = travelf)
                        dt = avs["mean", ]
                }
                else {
                        msk = data$correct & data$include & (which == data$condition)
                        if (msecf) {
                                if (travelf == 0)
                                        dt = data$latency.msec[msk] - data.analysis$problem.effs[data$problem[msk]]
                                if (travelf == 1)
                                        dt = data$wait.msec[msk]
                                if (travelf == 2)
                                        dt = data$travel.msec[msk]
                        }
                        else {
                                if (travelf == 0)
                                        dt = data$latency.ptile[msk]
                                if (travelf == 1)
                                        dt = data$wait.ptile[msk]
                                if (travelf == 2)
                                        dt = data$travel.ptile[msk]
                        }
                }
                newmath.mean.se(dt, zerof = TRUE)
        }
newmath.choose.problem <-
        function (t, possible.problems = names(data.analysis$problems))
        {
                if (t$status != "okay")
                        return(t)
                if (data.analysis$correction & t$correction.trial)
                        return(t)
                t$problem = sample(possible.problems, size = 1)
                t
        }
newmath.compare <-
        function (conds = 1:3, data = newmath5)
        {
                n = length(conds)
                cv = rainbow(n)
                t = options(scipen = 6)
                on.exit(options(t))
                u = rev(unique(data$condition[data$include]))
                ok1 = data$include & data$correct
                ok = (data$condition %in% u[conds]) & ok1
                t = oneway.test(data$latency.ptile ~ data$condition, subset = ok,
                                var.equal = TRUE)
                print(t)
                x.values = list()
                y.values = list()
                for (i in 1:n) {
                        m = (data$condition == u[i]) & ok1
                        xv = as.POSIXct(data$when[m])
                        xv = difftime(xv, xv[1], units = "mins")
                        x.values[[i]] = xv
                        y.values[[i]] = data$latency.ptile[m]
                }
                x.range = c(0, 0.4) + range(unlist(x.values))
                oldpar = par(las = 1, cex = 1.8)
                on.exit(par(oldpar))
                plot(x.values[[1]], y.values[[1]], xlab = "Time Since First Trial (min)",
                     ylab = "Percentile", xlim = x.range, ylim = c(0, 100),
                     type = "n")
                for (i in 1:n) {
                        points(x.values[[i]], y.values[[i]], pch = 16, col = cv[i])
                        lines(lowess(x.values[[i]], y.values[[i]]), lwd = 6,
                              col = cv[i])
                }
                legend("topright", legend = substr(u[conds], 12, 16), pch = 16,
                       col = cv, bty = "n")
                ttl = paste("Recent Tests (p = ", signif(t[["p.value"]],
                                                         1), ")", sep = "")
                title(ttl, line = 1)
        }
newmath.compare.all <-
        function (data=newmath5,n=100, minu = 5)
        {#print out the percentile of the latest average ptile to the previous n scores
                #
                # data dataset
                # n    number of conditions in comparison group
                # minu minumum number of conditions to compute
                #
                u=unique(data$condition[data$include])
                n=min(n,length(u)-1)
                if(n<minu) return()
                cond.inc=tail(u,n+1)
                rt=sapply(cond.inc,FUN=newmath.average,data=data)
                latest=tail(rt[1,],1)
                ptile=round(100*sum(latest>rt[1,-(n+1)],na.rm=TRUE)/(ncol(rt)-1), digits = 0)
                cat("compared to the previous",n,"conditions, the latest is",ptile,"%ile\n")
        }
newmath.compare.resp <-
        function (data = newmath5, condA = c(164:203, 222:270), condB = 204:221,
                  cv = rainbow(2), allresp = 2:8)
        {
                newmath.save.graphs()
                newmath.compare.resp.time(data = data, condA = condA, condB = condB,
                                          cv = cv, allresp = allresp)
                newmath.compare.resp.plot(measure = data$latency.ptile, measure.name = "Percentile",
                                          data = data, condA = condA, condB = condB, cv = cv, allresp = allresp)
                newmath.compare.resp.plot(measure = data$latency.msec, measure.name = "Reaction Time (msec)",
                                          data = data, condA = condA, condB = condB, cv = cv, allresp = allresp)
                newmath.compare.resp.plot(measure = data$correct, measure.name = "Error Rate (percent)",
                                          data = data, condA = condA, condB = condB, cv = cv, allresp = allresp)
        }
newmath.compare.resp.one <-
        function (resp = "5", conds = tail(unique(newmath5$condition[newmath5$include]),
                                           10), measure = newmath5$latency.ptile, data = newmath5)
        {
                if (is.logical(measure)) {
                        msk = (data$problem == resp) & (data$condition %in% conds) &
                                data$include
                        ans = newmath.wrong.compute(msk = msk, correct = data$correct,
                                                    sef = TRUE)
                }
                else {
                        msk = (data$problem == resp) & (data$condition %in% conds) &
                                data$correct & data$include
                        ans = newmath.mean.se(measure[msk])
                }
                ans
        }
newmath.compare.resp.plot <-
        function (measure = newmath5$latency.ptile, measure.name = "Percentile",
                  data = newmath5, condA = 101:200, condB = 201:300, cv = rainbow(2),
                  allresp = 2:8)
        {
                oldpar = par(las = 1, cex = 1.8)
                u = unique(data$condition[data$include])
                allresp = as.character(allresp)
                A.results = sapply(allresp, FUN = newmath.compare.resp.one,
                                   conds = u[condA], data = data, measure = measure)
                B.results = sapply(allresp, FUN = newmath.compare.resp.one,
                                   conds = u[condB], data = data, measure = measure)
                A.range = range(A.results[1, ] - A.results[2, ], A.results[1,
                                                                           ] + A.results[2, ])
                B.range = range(B.results[1, ] - B.results[2, ], B.results[1,
                                                                           ] + B.results[2, ])
                y.range = range(A.range, B.range)
                plot(allresp, A.results[1, ], xlab = "Stimulus", ylab = measure.name,
                     ylim = y.range, type = "n")
                points(allresp, A.results[1, ], pch = 16, col = cv[1], type = "b",
                       lwd = 3)
                segments(x0 = as.integer(allresp), y0 = A.results[1, ] -
                                 A.results[2, ], y1 = A.results[1, ] + A.results[2, ],
                         lwd = 2)
                points(allresp, B.results[1, ], pch = 16, col = cv[2], type = "b",
                       lwd = 3)
                segments(x0 = as.integer(allresp), y0 = B.results[1, ] -
                                 B.results[2, ], y1 = B.results[1, ] + B.results[2, ],
                         lwd = 2)
        }
newmath.compare.resp.time <-
        function (data = newmath5, condA = c(164:203, 222:270), condB = 204:221,
                  cv = rainbow(2), allresp = 2:8)
        {
                oldpar = par(las = 1, cex = 2)
                on.exit(par(oldpar))
                allcond = c(condA, condB)
                okay = data$include & data$correct & (data$problem %in% as.character(allresp))
                u = unique(data$condition[data$include])
                cond.inc = u[allcond]
                dts = newmath.when(conditions = cond.inc, data = data[okay,
                                                                      ])
                rt = sapply(cond.inc, FUN = newmath.average, data = data[okay,
                                                                         ])
                cv1 = rep(cv[1], times = length(allcond))
                cv1[allcond %in% condB] = cv[2]
                newmath.plot.bars(dts, rt, ttl = "Speed (%ile)", co = cv1,
                                  ylbl = "Reaction Time (percentile)")
        }
newmath.conds <-
        function (date = "2012-05-03", data = newmath5)
        {
                ok = data$condition[data$include]
                conds = unique(ok[substr(ok, 1, 10) == date])
                conds
        }
newmath.correct <-
        function (data = newmath3, condition = tail(newmath3$condition,
                                                    1))
        {
                data$latency.ptile[data$include & data$correct & (condition ==
                                                                          data$condition)]
        }
newmath.describe <-
        function ()
        {
        }
newmath.discrimination <-
        function (cutoff = 100, n = 20, sim = 50, alternative = 0)
        {
                conds = tail(unique(newmath5$condition[newmath5$include]),
                             -cutoff)
                omit = as.character(c(0, 2:8))
                tnms = c("ptile", alternative)
                nms = list(transform = tnms, omit = omit, sim = 1:sim)
                fvalues = array(NA, dim = c(2, length(omit), sim), dimnames = nms)
                if (alternative == 0)
                        trans = log(newmath5$latency.msec)
                else trans = newmath5$latency.msec^alternative
                all = NULL
                for (sa in 1:sim) {
                        sam.msk = (newmath5$condition %in% sample(conds, size = n)) &
                                newmath5$correct & newmath5$include
                        for (om in omit) {
                                for (tr in tnms) {
                                        msk = sam.msk & (newmath5$problem != om)
                                        if (tr == "ptile")
                                                d = newmath5$latency.ptile
                                        else d = trans
                                        fvalues[tr, om, sa] = newmath.discrimination.f(d,
                                                                                       msk)
                                }
                        }
                        all = c(all, fvalues[1, 1, sa])
                        fvalues[, , sa] = 100 * fvalues[, , sa]/fvalues[1, 1,
                                                                        sa]
                }
                print(mean.se(all))
                newmath.discrimination.plot(fvalues)
        }
newmath.discrimination.f <-
        function (data = newmath5$latency.ptile, msk = NA)
        {
                t = lm(data ~ as.factor(newmath5$problem) + as.factor(newmath5$condition),
                       subset = msk)
                t1 = summary(aov(t))
                (t1[[1]])[2, 4]
        }
newmath.discrimination.plot <-
        function (a, cv = c("blue", "brown"))
        {
                oldpar = par(las = 1, cex = 1.8)
                on.exit(par(oldpar))
                s = apply(a, FUN = mean.se, MAR = c("transform", "omit"))
                rng = range(s["mean", , ] + s["se", , ], s["mean", , ] -
                                    s["se", , ])
                dn = dimnames(s)
                o = as.integer(dn$omit)
                tran = dn$transform[2]
                plot(o, s["mean", "ptile", ], ylim = rng, xlab = "Problem Omitted",
                     ylab = "F Value (% baseline)", type = "n")
                points(o, s["mean", "ptile", ], pch = 16, col = cv[1])
                points(o, s["mean", tran, ], pch = 16, col = cv[2])
                segments(x0 = o, y0 = s["mean", "ptile", ] - s["se", "ptile",
                                                               ], y1 = s["mean", "ptile", ] + s["se", "ptile", ])
                segments(x0 = o, y0 = s["mean", tran, ] - s["se", tran, ],
                         y1 = s["mean", tran, ] + s["se", tran, ])
        }
newmath.end.session <-
        function (t, old.data = newmath6, measure.travel = TRUE)
        {
                if (t$note == "warmup")
                        msg1 = "warmup done"
                else msg1 = "all done"
                msg2 = paste("total time", round(difftime(Sys.time(), data.analysis$start.time,
                                                          unit = "mins"), 1), "minutes\n")
                newmath.paint(above = msg1, below = msg2, duration = 3)
                if ((t$status == "end session") | (t$note == "warmup"))
                        dataset$include <<- FALSE
                new.data = rbind(old.data, tail(dataset, -1))
                new.data = newmath.set.types(new.data, measure.travel = measure.travel)
                if ((t$status == "end session") | (t$note == "warmup"))
                        return(new.data)
                new.data = newmath.analyze(new.data, measure.travel = measure.travel)
                invisible(new.data)
        }
newmath.evaluate.answer <-
        function (t = list(status = "okay", problem = "4", actual.answer = "4",
                           latency.msec = 500, note = ""), measure.travel = FALSE)
        {
                if (t$status != "okay")
                        return(t)
                t$correct = NA
                t$latency.ptile = NA
                t$include = FALSE
                if (measure.travel) {
                        t$wait.ptile = NA
                        t$travel.ptile = NA
                }
                if (!t$actual.answer %in% data.analysis$problems) {
                        t$correct = FALSE
                        t$status = "illegal answer"
                        t$note = "illegal character"
                        t$correction.trial = TRUE
                        return(t)
                }
                t$correct = data.analysis$problems[t$problem] == t$actual.answer
                if (t$latency.msec <= data.analysis$too.fast.msec) {
                        t$status = "illegal answer"
                        t$note = "too fast"
                        return(t)
                }
                if (t$latency.msec >= data.analysis$too.slow.msec) {
                        t$status = "illegal answer"
                        t$note = "too slow"
                        return(t)
                }
                t$include = TRUE
                if (t$correction.trial & data.analysis$correction) {
                        t$include = FALSE
                        t$note = "correction trial"
                }
                if (t$correct) {
                        if (!t$correction.trial) {
                                t$latency.ptile = newmath.ptile(t$problem, t$latency.msec,
                                                                data.analysis$latency.ptile.comparisons)
                                data.analysis$latency.ptiles.so.far <<- c(data.analysis$latency.ptiles.so.far,
                                                                          t$latency.ptile)
                                t$average.latency.ptile = mean(data.analysis$latency.ptiles.so.far)
                                if (measure.travel) {
                                        t$wait.ptile = newmath.ptile(t$problem, t$wait.msec,
                                                                     data.analysis$wait.ptile.comparisons)
                                        data.analysis$wait.ptiles.so.far <<- c(data.analysis$wait.ptiles.so.far,
                                                                               t$wait.ptile)
                                        t$average.wait.ptile = mean(data.analysis$wait.ptiles.so.far)
                                        t$travel.ptile = newmath.ptile(t$problem, t$travel.msec,
                                                                       data.analysis$travel.ptile.comparisons)
                                        data.analysis$travel.ptiles.so.far <<- c(data.analysis$travel.ptiles.so.far,
                                                                                 t$travel.ptile)
                                        t$average.travel.ptile = mean(data.analysis$travel.ptiles.so.far)
                                }
                        }
                }
                t
        }
newmath.export.save.ws <-
        function (nm="braintest")
        {#this is the newmath.save.ws used when the functions are exported
                #by newmath.export
                #
                .SavedPlots=NULL
                lo=paste(getwd(),"/",nm,".RData",sep="")
                save.image(lo)
                cat(nm,"workspace saved at",lo,"\n")
                cat(as.character(Sys.time()),"\n")
        }
newmath.feedback <-
        function (t = list(status = "okay", latency.msec = 500, latency.ptile = 50,
                           average.ptile = 30, correct = TRUE, note = ""))
        {
                z = NULL
                if (t$status == "end session") {
                        if (!is.null(data.analysis$feedback.offset))
                                z = data.analysis$feedback.offset[t$status]
                        newmath.paint(paste("session ended", z, sep = ""), duration = 1.5)
                        return()
                }
                if (t$status == "cancel trial") {
                        if (!is.null(data.analysis$feedback.offset))
                                z = data.analysis$feedback.offset[t$status]
                        newmath.paint(paste("trial cancelled", z, sep = ""),
                                      duration = 1.5)
                        return()
                }
                if (t$note %in% c("too fast", "too slow", "illegal character")) {
                        if (!is.null(data.analysis$feedback.offset))
                                z = data.analysis$feedback.offset[t$note]
                        newmath.paint(paste(t$note, z, sep = ""), duration = 1.5)
                        return()
                }
                if (!t$correct) {
                        if (!is.null(data.analysis$feedback.offset))
                                z = data.analysis$feedback.offset["wrong"]
                        newmath.paint(paste("wrong", z, sep = ""), duration = 1)
                }
                else {
                        if (is.null(t$average.latency.ptile))
                                t$average.latency.ptile = NA
                        lpt = round(t$latency.ptile)
                        alp = round(t$average.latency.ptile)
                        ptile.msg = paste(lpt, "%ile", sep = "")
                        top = paste(t$latency.msec, "ms", ptile.msg)
                        bottom = paste(alp, "%ile", sep = "")
                        if (!is.null(data.analysis$feedback.offset)) {
                                top = paste(top, data.analysis$feedback.offset["top"])
                                bottom = paste(bottom, data.analysis$feedback.offset["bottom"])
                        }
                        newmath.paint(center = top, center.size = 4, below = bottom,
                                      below.size = 3, col = "blue", duration = 1)
                }
        }
newmath.foreperiod <-
        function (t = list(status = "okay"), text.size = 6)
        {
                if (t$status != "okay")
                        return(t)
                if (is.null(data.analysis$center.bar))
                        center.bar = "|"
                else {
                        center.bar = data.analysis$center.bar
                        text.size = data.analysis$text.size[2]
                }
                newmath.paint(center.bar, duration = data.analysis$foreperiod.msec/1000,
                              center.size = text.size)
        }
newmath.get.key <-
        function (key)
        {
                key
        }
newmath.instructions <-
        function ()
        {# instructions
                #
                # Big Picture
                #
                # There are three central functions
                #
                #    run.warmup     short version of main test
                #    run.test       main test
                #    see.results    makes many graphs of results
                #
                # The results are stored in a variable (which has data type = data frame, similar to a table)
                # called
                #
                #    newmath5
                #
                #
                # Basic Instructions
                #
                # 1. Before a daily test, do two warmup sessions (each lasts until 6
                #    correct answers have been made). The warmup sessions
                #    reduce the effect of missing days and make it easier to compare two
                #    tests in a row (e.g., you do one test at 4:00 pm and another test at 4:05 pm)
                #    You do a warmup session by typing
                #   
                #    run.warmup()
                #
                # 2. Start the main test program by typing
                #
                #    run.test()
                #
                #    The first question, before the first trial, asks you
                #    to describe the session in words ("describe this condition"). If you are
                #    doing an experiment describe the current treatment (e.g., "baseline phase",
                #    "20 g of Dove dark chocolate daily" -- give plenty of details). If you are
                #    not doing an experiment, describe anything unusual about the day so far and/or
                #    the test conditions. This description can be as long (as many characters) as you want.
                #    Include anything unusual you think might affect the results. For example, "had milkshake
                #    at 10 am", "test later than usual, 6 pm rather than 4 pm", "test during train
                #    ride".
                #
                # 3. Put your index finger (either hand) on the 5 key. You start each trial
                #    by pressing the 5 key.
                #
                # 4. When you see a digit (e.g., 4), type that digit as fast as possible
                #    with your index finger -- the same finger you used to press 5.
                #    The possible digits are 2, 3, 4, 5, 6, 7, and 8. The 7 possible
                #    digits are equally likely.
                # 
                # 5. After a wrong answer there is a "correction" trial -- a trial with
                #    the same digit that doesn't count in the final results.
                #
                # 6. A test session lasts until 32 correct answers have been made. Wrong
                #    answers are not used in the analyses, except to compute percent correct.
                #
                # 7. After you finish a test session you will see a lot of graphs (after you
                #    have collected sufficient data) flash by. You can scroll back and forth through
                #    these graphs using the PgUp and PgDn keys. To see the graphs again type
                #  
                #    newmath5.plot()
                #
                # 8. To return to the menu system, type
                #   
                #    me()
                #
                # 9. When you end a session you will be asked "save workspace?" Answer no. The
                #    workspace has already been saved.
                #
                # Full Instructions
                #
                # After each test (about 32 problems) the function newmath.save.ws
                # saves your data. 
                #
                # To start the main test program, type
                #
                #   run.test()
                #
                # If the start up window ("press 5 to start...") does not fill the
                # whole screen, expand it (via the Windows expansion button in the upper
                # right corner of the window).
                #
                # While doing the test respond as fast as possible while making few mistakes.
                # It's hard to know what the best error rate is, but it certainly is greater
                # than zero. Pick an error rate that you find comfortable.
                #
                # While running the program you press 5 to get the next trial.
                # If you want to end the session, type "e". If something went wrong while
                # doing the trial you've just finished, type "c" to cancel it.
                #
                # When program ends it stores the data automatically to the location
                # given in newmath.save.ws.
                #
                # The next time you run you must use the workspace stored by newmath.save.ws.
                #
                # During a trial you get "percentile feedback" -- comparison of your reaction
                # time to previous reaction times in percentile terms. If your reaction time
                # is at the median of previous times, the percentile is 50. Faster reaction times
                # = higher percentile. The percentile in large type is the percentile for
                # the trial just finished. The number in smaller type below that (e.g., 30%ile)
                # is the mean of the percentiles for all the trials
                # so far that session. There is no percentile given for correction trials (that
                # is, it is shown as NA = not available).
                #
                # When you start using the program it is "preloaded" with reaction times. As
                # you use the program, your reaction times will gradually replace the preloaded
                # ones, improving the comparison of current trials to previous trials.
                #
                # To see the contents of newmath, type
                #
                # newmath5
                #
                # which shows the whole thing or
                #
                # tail(newmath5)
                #
                # which shows only the most recent 5 rows.
                #
                # In the beginning the percentile will probably be 0 because you are slow relative
                # to someone experienced. As more and more of your data enters the system, the
                # percentile will more and more compare your current speed to your earlier speeds.
                #
                # In the very beginning, a summary percentile may be NaN (= not a number) because
                # there is too little data. The NaN will go away when you have collected more data.
                #
                # The timezone may be wrong, but it doesn't seem to matter. You can find out
                # the system time with Sys.time() and the current timezone with Sys.timezeon().
        }
newmath.make.name <-
        function (warmupf = FALSE)
        {
                t = as.character(Sys.time())
                t = substr(t, 1, 16)
                substr(t, 5, 5) = "."
                substr(t, 8, 8) = "."
                if (warmupf)
                        t = paste(t, "warmup")
                t
        }
newmath.me <-
        function () {
                menu.choices <- c("Begin warmup session",
                                  "Begin testing session",
                                  "Show instructions",
                                  "Show plots",
                                  "Export all trial data to CSV file",
                                  "Export only valid trial data to CSV file",
                                  "Exit to R command prompt",
                                  "Exit to Windows")
                
                switch(menu(menu.choices, graphics = TRUE, title = "Brain Tracking Program"),
                       run.warmup(),
                       run.test(),
                       print(newmath.instructions),
                       see.results(),
                       write.csv(newmath5, row.names=FALSE, file="brain_data_raw.csv"),
                       write.csv(newmath5[which(newmath5$correct==TRUE & newmath5$include==TRUE),], row.names=FALSE, file="brain_data_only_valid.csv"),
                       print("to return to menu, type me()"),
                       quit(save = "no"))
        }
newmath.mean.se <-
        function (v, trim = 0, zerof = FALSE)
        {
                v = v[!is.na(v)]
                r = c(mean = mean(v, na.rm = TRUE, trim = trim), se = newmath.se(v,
                                                                                 zerof = zerof), n = length(v))
                r
        }
newmath.name.session <-
        function (data = newmath5, label = "")
        {
                base.name = substr(Sys.time(), 1, 16)
                if (label == "warmup")
                        return(paste(base.name, label))
                cat("current time", as.character(Sys.time()), "\n")
                cat("most recent condition", tail(data$condition[data$include],
                                                  1), "\n")
                cat("describe this condition")
                paste(base.name, label, scan(nlines = 1, what = "character",
                                             quiet = TRUE, sep = "!"))
        }
newmath.new.condition <-
        function (data = newmath3)
        {
                cat("current time", as.character(Sys.time()), "\n")
                cat("most recent condition", tail(data$condition, 1), "\n")
                cat("this condition")
                condition = scan(nlines = 1, what = "character", quiet = TRUE,
                                 sep = "!")
                condition
        }
newmath.order <-
        function (data = newmath5)
        {
                ok = data$include & data$correct
                expt = "first" == substring(data$condition, 27, 31)
                msk = expt & ok
                ord = "5" == substring(data$condition, 25, 25)
                t = aov(latency.msec ~ as.factor(problem) + ord + as.factor(condition) %in%
                                ord, data = data, subset = msk)
                print(anova(t))
                cat("newmath5 first mean msec", mean.se(data$latency.msec[ord &
                                                                                  msk]), "\n")
                cat("newmath6 first mean msec", mean.se(data$latency.msec[(!ord) &
                                                                                  msk]), "\n")
        }
newmath.paint <-
        function (center = "", above = "", below = "", bottom = "", x = 0,
                  y = 0, xlim = c(-1, 1), ylim = c(-1, 1), text.size = 0, above.size = 4,
                  center.size = 4, below.size = 3, bottom.size = 2, duration = 1,
                  col = "red", new = TRUE)
        {
                if (new)
                        plot(0, 0, xlab = "", ylab = "", xaxt = "n", yaxt = "n",
                             type = "n", xlim = xlim, ylim = ylim)
                if (text.size > 0) {
                        above.size = text.size
                        center.size = text.size
                        below.size = text.size
                }
                text(center, x = x, y = y, cex = center.size, col = col)
                text(above, x = x, y = y + 0.33, cex = above.size, col = col)
                text(below, x = x, y = y - 0.33, cex = below.size, col = col)
                text(bottom, x = x, y = y - 0.85, cex = bottom.size, col = col)
                Sys.sleep(duration)
        }
newmath.plot <-
        function (last.cond = 20, dayf=FALSE, data=newmath5,cv = rainbow(2), min.cond = 3, recentf=TRUE, distributionf=FALSE, travelf=FALSE,linesf=FALSE, line.one)
        {#plot newmath results. Percent wrong vs date, adjusted RT vs date.
                #
                # last.cond  if length = 1, number of conditions to plot (from most recent). If larger
                #            than the number available, all conditions are plotted. If length>1,
                #            indicates the numbers of conditions to plot. If length = 2 or more, indicates
                #            condition numbers to plot. E.g., if last.cond = 5:20, plot conditions
                #            5 to 20.
                # dayf       plot one average per day (with se = variation across conditions)
                # data       dataset
                # cv         vector of colors
                # recentf    compare recent conditions?
                # distributionf show distributions?
                # travelf    plot travel times?
                # linesf     plot two fitted lines? 2nd line starts where first line ends
                # line.one   index numbers of first line
                # min.cond   don't plot ptiles until number of conditions reaches this
                #
                oldpar=par(las = 1, cex = 2)
                on.exit(par(oldpar))
                #
                # choose conditions
                okay=data$include&data$correct
                u=unique(data$condition[okay])
                if(length(last.cond)<2)cond.inc=tail(u,last.cond)
                else cond.inc=u[last.cond]
                which=newmath.specify(cond.inc,dayf=dayf)
                dts=newmath.when(which=which,data=data[okay,])
                rt=sapply(which,FUN=newmath.average, data=data[okay,], travelf=travelf)
                rt.msec=sapply(which,FUN=newmath.average, data=data[okay,], msecf=TRUE, travelf=travelf)
                pw=sapply(which, FUN=newmath.wrong, data=data, sef=TRUE)
                #
                # accuracy plot
                if(!travelf) newmath.plot.bars(dts,pw,ttl="Accuracy",co=cv[2],ylbl="Percent Wrong", linesf=linesf,line.one=line.one)
                #
                # distribution plot
                #if(distributionf&!travelf) newmath.plot.boxplot(conditions=cond.inc,data=data)
                #
                # comparison plot (compare most recent 4 conditions)
                if((4<=length(u))&recentf&!travelf) newmath.compare(1:4, data=data)
                #
                # speed plot
                #if(travelf) ylbl="Travel Time (log msec)"
                ylbl="Reaction Time (msec)"
                newmath.plot.bars(dts,rt.msec,ttl="Speed (msec)",co=cv[1],ylbl=ylbl)
                #if(travelf==2) ylbl="Travel Time (ptile)"
                ylbl="Reaction Time (ptile)"
                if(min.cond<=length(u))newmath.plot.bars(dts,rt,ttl="Speed (%ile)",co=cv[1],ylbl=ylbl, linesf=linesf, line.one=line.one)
                #
                # print results
                colnames(rt)=substr(colnames(rt),1,50)
                data.frame(ptile=round(rt[1,]), msec=round(rt.msec[1,],1), pcnt.wrong=as.integer(pw[1,]))
        }
newmath.plot.bars <-
        function (dts,mean.and.se,ttl="",ylbl,co="red",x.label="Date",yticks,newf=TRUE, linesf=FALSE, line.one)
        {# make main graphs
                #
                #  dts    dates (suitable for plotting)
                #  mean.and.se  mean, 1st row, se, 2nd row
                #  ttl    title
                #  ylbl   y axis label
                #  yticks location of y axis ticks (if present, indicates negative
                #         reciprocal transformation)
                #  newf   new plot?
                #  linesf add 2 fitted lines?
                #  line.one  index nums of first line
                #
                mean.and.se[2,is.na(mean.and.se[2,])]=0
                yv=mean.and.se[1,]
                up=yv+mean.and.se[2,];lo=yv-mean.and.se[2,]
                time.span.hr=as.numeric(difftime(tail(dts,1),dts[1],unit="hours"))
                if(time.span.hr<24) x.label="Time of Day"
                if(newf) {
                        r=range(up,lo,na.rm=TRUE)
                        if(missing(yticks))plot(dts,yv,ylim=r,xlab = x.label,ylab = ylbl,type="n")
                        else {
                                plot(dts,yv,ylim=r,xlab = x.label,ylab = ylbl,type="n", yaxt="n")
                                axis(2,at=-1/yticks,labels=as.character(yticks))
                        }
                }
                points(dts,yv,col = co,pch = 16, cex = 1.5)
                segments(dts,up,dts,lo,lwd=1,lty=2)
                if(linesf){
                        lines(loess.smooth(dts[line.one],yv[line.one]),col="green",lwd=4)
                        last=length(dts)
                        first=1+max(line.one)
                        lines(loess.smooth(dts[first:last],yv[first:last]),col="blue",lwd=4)
                }
                if(newf) abline(h=median(mean.and.se[1,],na.rm=TRUE),lwd = 3,lty = 2)
                if(newf) title(ttl)
        }
newmath.plot.both <-
        function (days.back = 1, cv = c("blue", "green"))
        {
                oldpar = par(cex = 2, las = 1)
                on.exit(par(oldpar))
                n3.conds = newmath.plot.both.conditions(data = newmath3,
                                                        days.back = days.back)
                n4.conds = newmath.plot.both.conditions(data = newmath4,
                                                        days.back = days.back)
                n3.dts = newmath.when(conditions = n3.conds, data = newmath3)
                n4.dts = newmath.when(conditions = n4.conds, data = newmath4)
                n3.rt = sapply(n3.conds, FUN = newmath.average, data = newmath3)
                n4.rt = sapply(n4.conds, FUN = newmath.average, data = newmath4)
                x.range = range(n3.dts, n4.dts)
                y.range = range(n3.rt[1, ] + n3.rt[2, ], n3.rt[1, ] - n3.rt[2,
                                                                            ], n4.rt[1, ] + n4.rt[2, ], n4.rt[1, ] - n4.rt[2, ])
                plot(n3.dts, n3.rt[1, ], xlim = x.range, ylim = y.range,
                     type = "n", xlab = "Date", ylab = "Speed (percentile)")
                newmath.plot.bars(n3.dts, n3.rt, co = cv[1], newf = FALSE)
                newmath.plot.bars(n4.dts, n4.rt, co = cv[2], newf = FALSE)
                legend("topright", pch = 16, legend = c("number", "letter"),
                       col = cv, bty = "n")
                title("Numbers and Letters Together")
        }
newmath.plot.both.conditions <-
        function (data = newmath3, days.back = 1)
        {
                ok = data$include & data$correct
                u = unique(data$condition[ok])
                d = Sys.time() - newmath.when(data = data, conditions = u)
                units(d) = "days"
                u[d < days.back]
        }
newmath.plot.boxplot <-
        function (conditions = tail(unique(newmath3$condition[newmath3$include]),
                                    30), data = newmath3)
        {
                di = lapply(conditions, FUN = newmath.correct, data = data)
                boxplot(di, col = "green", ylab = "Solution Time (percentile)",
                        xlab = "Condition")
        }
newmath.plot.identify <-
        function (data = newmath5, last.cond = 40, cv = rainbow(2))
        {
                oldpar = par(las = 1, cex = 2)
                on.exit(par(oldpar))
                okay = data$include & data$correct
                u = unique(data$condition[okay])
                if (length(last.cond) < 2)
                        last.cond = tail(1:length(u), last.cond)
                cond.inc = u[last.cond]
                rt = sapply(cond.inc, FUN = newmath.average, data = data)
                plot(last.cond, rt[1, ], xlab = "Condition Number", pch = 16,
                     col = cv[1], ylab = "Reaction Time (percentile)")
                identify(x = last.cond, y = rt[1, ], labels = as.character(last.cond))
        }
newmath.problem <-
        function (problem = "4", status = "okay")
        {
                newmath.show(problem)
                see.time = Sys.time()
                actual.answer = getGraphicsEvent(prompt = "", onKeybd = get.key)
                resp.time <<- Sys.time()
                answer.msec = as.integer(1000 * difftime(resp.time, see.time,
                                                         unit = "sec"))
                answer.ptile = NA
                if (!actual.answer %in% data.analysis$possible.answers) {
                        answer.msec = NA
                        correct = NA
                        include = FALSE
                        status = "answer out of bounds"
                }
                else {
                        actual.answer = toupper(actual.answer)
                        correct = actual.answer == problem
                        if (correct)
                                answer.ptile = newmath.ptile(problem = problem, answer.msec = answer.msec)
                        include = TRUE
                        status = "okay"
                }
                list(answer.msec = answer.msec, answer.ptile = answer.ptile,
                     actual.answer = actual.answer, correct = correct, include = include,
                     status = status)
        }
newmath.problem.effs <-
        function (val = newmath5$latency.msec[newmath5$include & newmath5$correct],
                  pr = newmath5$problem[newmath5$include & newmath5$correct],
                  problems = newmath5.analysis$problems)
        {
                effs = numeric(length(problems))
                names(effs) = names(problems)
                for (nm in names(effs)) effs[nm] = mean(val[nm == pr])
                effs = effs - mean(effs)
                effs
        }
newmath.ptile <-
        function (problem = "2", value = 400, comp = data.analysis$latency.ptile.comparisons)
        {
                n = nrow(comp)
                if (n < 5)
                        return(NA)
                a = sum(value < comp[, as.character(problem)]) + sum(value ==
                                                                             comp[, as.character(problem)])/2
                unit = 1/(2 * n)
                100 * (unit + a * ((1 - unit) - unit)/n)
        }
newmath.put <-
        function (..., file = "C:\\Users\\Seth\\Documents\\temp0.RData")
        {
                save(..., file = file)
        }
newmath.record.trial <-
        function (t = list(status = "okay"), measure.travel = FALSE)
        {
                if (t$status == "cancel trial") {
                        nr = nrow(dataset)
                        dataset$include[nr] <<- FALSE
                        dataset$note[nr] <<- "trial cancelled"
                        if (dataset$correct[nr])
                                t$trial = t$trial - 1
                        data.analysis$latency.ptiles.so.far <<- head(data.analysis$latency.ptiles.so.far,
                                                                     -1)
                        t$average.latency.ptile = mean(data.analysis$latency.ptiles.so.far)
                        if (measure.travel) {
                                data.analysis$wait.ptiles.so.far <<- head(data.analysis$wait.ptiles.so.far,
                                                                          -1)
                                t$average.wait.ptile = mean(data.analysis$wait.ptiles.so.far)
                                data.analysis$travel.ptiles.so.far <<- head(data.analysis$travel.ptiles.so.far,
                                                                            -1)
                                t$average.travel.ptile = mean(data.analysis$travel.ptiles.so.far)
                        }
                        return(t)
                }
                if (t$status == "end session") {
                        nr = nrow(dataset)
                        dataset$include <<- FALSE
                        dataset$note[nr] <<- "session ended"
                        return(t)
                }
                new.line1 = c(as.character(Sys.time()), t$condition, t$trial,
                              data.analysis$foreperiod.msec, t$problem)
                new.line2 = c(t$latency.msec, t$latency.ptile[1])
                if (measure.travel)
                        new.line2 = c(t$wait.msec, t$wait.ptile[1], t$travel.msec,
                                      t$travel.ptile[1], new.line2)
                new.line3 = c(t$actual.answer, t$correct, t$include, t$note)
                dataset <<- rbind(dataset, c(new.line1, new.line2, new.line3))
                dataset <<- newmath.set.types(dataset, measure.travel = measure.travel)
                t
        }
newmath.remove.last.condition <-
        function (data=newmath5, note="condition excluded")
        {#set all entries for most recent condition to include = FALSE, add note.
                #Returns fixed dataset.
                #
                #  data dataset
                #  note give reason for exclusion
                #
                cond=tail(data$condition[data$include],1)
                msk=cond==data$condition
                data$include[msk]=FALSE
                data$note[msk]=note
                cat(tail(cond,1),note,"\n")
                invisible(data)
        }
newmath.save.graphs <-
        function ()
        {
                #windows.options(record=TRUE)
                .SavedPlots<<-NULL
                
        }
newmath.save.ws <-
        function (nm="braintest")
        {#this is the newmath.save.ws used when the functions are exported
                #by newmath.export
                #
                .SavedPlots=NULL
                lo=paste(getwd(),"/",nm,".RData",sep="")
                save.image(lo)
                cat(nm,"workspace saved at",lo,"\n")
                cat(as.character(Sys.time()),"\n")
        }
newmath.se <-
        function (v, zerof = FALSE)
        {
                v = v[!is.na(v)]
                t = sd(v)/sqrt(length(v))
                if (zerof) {
                        if (is.na(t))
                                return(0)
                }
                t
        }
newmath.sensitivity <-
        function ()
        {
        }
newmath.set.comparisons <-
        function (data = newmath5[newmath5$include & newmath5$correct,
                                  ], measure.travel = FALSE)
        {
                data.analysis$latency.ptile.comparisons <<- newmath.analyze.comparisons(val = data$latency.msec,
                                                                                        pr = data$problem)
                data.analysis$problem.effs <<- newmath.problem.effs(val = data$latency.msec,
                                                                    pr = data$problem, problems = data.analysis$problems)
                if (measure.travel) {
                        data.analysis$wait.ptile.comparisons <<- newmath.analyze.comparisons(val = data$wait.msec,
                                                                                             pr = data$problem)
                        data.analysis$travel.ptile.comparisons <<- newmath.analyze.comparisons(val = data$travel.msec,
                                                                                               pr = data$problem)
                }
                cat("comparisons set\n")
        }
newmath.set.types <-
        function (dataset, measure.travel = FALSE)
        {
                dataset$when = as.character(dataset$when)
                dataset$condition = as.character(dataset$condition)
                dataset$trial = as.integer(dataset$trial)
                dataset$foreperiod.msec = as.integer(as.character(dataset$foreperiod.msec))
                dataset$problem = as.character(dataset$problem)
                dataset$latency.msec = as.integer(dataset$latency.msec)
                dataset$latency.ptile = as.numeric(dataset$latency.ptile)
                if (measure.travel) {
                        dataset$wait.msec = as.integer(dataset$wait.msec)
                        dataset$wait.ptile = as.numeric(dataset$wait.ptile)
                        dataset$travel.msec = as.integer(dataset$travel.msec)
                        dataset$travel.ptile = as.numeric(dataset$travel.ptile)
                }
                dataset$actual.answer = as.character(dataset$actual.answer)
                dataset$correct = as.logical(dataset$correct)
                dataset$include = as.logical(dataset$include)
                dataset$note = as.character(dataset$note)
                dataset
        }
newmath.shortterm.practice <-
        function (last.cond = 300, data = newmath5, window = 360)
        {
                newmath.save.graphs()
                oldpar = par(las = 1, cex = 2)
                on.exit(par(oldpar))
                okay = data$include & data$correct
                u = unique(data$condition[okay])
                cat(length(u), "conditions available\n")
                if (length(last.cond) < 2)
                        cond.inc = tail(u, last.cond)
                else cond.inc = u[last.cond]
                dts = newmath.when(conditions = cond.inc, data = data[okay,
                                                                      ])
                rt = sapply(cond.inc, FUN = newmath.average, data = data)
                pw = newmath.wrong(data = data, conditions = cond.inc, sef = TRUE)
                t = newmath.shortterm.practice.diffs(when = dts, measure = rt[1,
                                                                              ], window)
                plot(log(t$x), t$y, xlab = "Time Difference (log min)", ylab = "Performance Difference")
                lines(loess.smooth(x = log(t$x), y = t$y, span = 1/4, degree = 2),
                      col = "green", lwd = 6)
                title("Speed (percentile)", line = 1)
                plot(t$x, t$y, xlab = "Time Difference (min)", ylab = "Performance Difference")
                lines(loess.smooth(x = t$x, y = t$y, span = 1/4, degree = 2),
                      col = "green", lwd = 6)
                title("Speed (percentile)", line = 1)
                t = newmath.shortterm.practice.diffs(when = dts, measure = pw[1,
                                                                              ], window)
                plot(log(t$x), t$y, xlab = "Time Difference (log min)", ylab = "Performance Difference")
                lines(loess.smooth(x = log(t$x), y = t$y, span = 1/4, degree = 2),
                      col = "blue", lwd = 6)
                title("Accuracy (percent wrong)", line = 1)
                plot(t$x, t$y, xlab = "Time Difference (min)", ylab = "Performance Difference")
                lines(loess.smooth(x = t$x, y = t$y, span = 1/4, degree = 2),
                      col = "blue", lwd = 6)
                title("Accuracy (percent wrong)", line = 1)
        }
newmath.shortterm.practice.diffs <-
        function (when, measure, window = 360)
        {
                x = NULL
                y = NULL
                n = length(when)
                for (i in 1:(n - 1)) {
                        for (j in (i + 1):n) {
                                d = difftime(when[j], when[i], units = "mins")
                                if (d > window)
                                        break
                                else {
                                        x = c(x, d)
                                        y = c(y, measure[j] - measure[i])
                                }
                        }
                }
                list(x = x, y = y)
        }
newmath.show <-
        function (number = "3", col = "green")
        {
                paint(number, duration = 0, col = col, text.size = 6)
        }
newmath.show.problem <-
        function (t = list(status = "okay", problem = "9"), col = "green",
                  text.size = 6)
        {
                if (t$status != "okay")
                        return(t)
                if (is.null(data.analysis$screen))
                        show = t$problem
                else {
                        show = data.analysis$screen[t$problem == names(data.analysis$screen)]
                        text.size = data.analysis$text.size[3]
                }
                newmath.paint(show, duration = 0, col = col, text.size = text.size)
        }
newmath.specify <-
        function (conds = tail(newmath5$condition, 1), dayf = FALSE)
        {
                if (!dayf)
                        return(conds)
                unique(substr(conds, 1, 10))
        }
newmath.start.trial <-
        function (t = list(trial = 3, total.trials = 20), prompt1 = "press space",
                  prompt2 = "to start", prompt3 = "", beepf = FALSE, prompt4 = "press e to end session, c to cancel previous trial",
                  col = "brown", text.size = 5)
        {
                prompt3 = paste("trial", t$trial, "of", t$total.trials)
                if (data.analysis$correction & t$correction.trial)
                        prompt3 = "correction trial"
                if (!is.null(data.analysis$prompt))
                        prompt1 = data.analysis$prompt
                if (!is.null(data.analysis$prompt.offset)) {
                        prompt1 = paste(prompt1, data.analysis$prompt.offset["prompt1"])
                        prompt2 = paste(prompt2, data.analysis$prompt.offset["prompt2"])
                        prompt3 = paste(prompt3, data.analysis$prompt.offset["prompt3"])
                        prompt4 = paste(prompt4, data.analysis$prompt.offset["prompt4"])
                }
                if (!is.null(data.analysis[["text.size"]]))
                        text.size = data.analysis$text.size[1]
                newmath.paint(center = prompt2, above = prompt1, below = prompt3,
                              bottom = prompt4, text.size = text.size, col = col)
                t$status = "get answer"
                while (t$status == "get answer") {
                        ans = getGraphicsEvent(prompt = "", onKeybd = newmath.get.key)
                        if (beepf)
                                beep()
                        if (ans %in% c(" ", "z", "5", "b"))
                                t$status = "okay"
                        if (ans == "c")
                                t$status = "cancel trial"
                        if (ans == "e")
                                t$status = "end session"
                }
                t
        }
newmath.take <-
        function ()
        {
                load(file = "C:\\Users\\Seth\\Documents\\temp0.RData", .GlobalEnv)
        }
newmath.trial <-
        function (trial = 1, problem = names(newmath3.analysis$problem)[5],
                  condition = "testing", wait.msec = 1500, note = "")
        {
                tn = paste("trial", trial, "of", total.trials)
                msg = press.space.to.start(below = tn, col = "brown")
                if (msg != "ok")
                        result = list(status = msg)
                else {
                        paint("|", duration = wait.msec/1000)
                        t = newmath.problem(problem = problem)
                        newmath.feedback(problem = problem, answer.msec = t$answer.msec,
                                         answer.ptile = t$answer.ptile, correct = t$correct,
                                         status = t$status)
                        result = list(wait.msec = wait.msec, answer.msec = t$answer.msec,
                                      answer.ptile = t$answer.ptile[1], actual.answer = t$actual.answer,
                                      correct = t$correct, include = t$include, status = t$status)
                }
                result
        }
newmath.trial.average <-
        function (data = newmath5, conds = "all")
        {
                u = unique(data$condition[data$include])
                if (conds == "all")
                        cv = u
                else cv = u[conds]
                num.trials = max(data$trial) - 1
                m.se = matrix(NA, ncol = 2, nrow = num.trials)
                for (tr in 2:num.trials) {
                        msk = data$include & (data$condition %in% cv) & data$correct &
                                (tr == data$trial)
                        m.se[tr, ] = newmath.mean.se(data$latency.ptile[msk])
                }
                oldpar = par(las = 1, cex = 1.4)
                on.exit(par(oldpar))
                upper = m.se[-1, 1] + m.se[-1, 2]
                lower = m.se[-1, 1] - m.se[-1, 2]
                plot(2:num.trials, m.se[-1, 1], xlab = "Trial", ylab = "Speed (percentile)",
                     ylim = range(upper, lower), pch = 16, cex = 2.5, col = "green")
                segments(2:num.trials, lower, 2:num.trials, upper, lty = 2,
                         lwd = 1)
                abline(h = median(m.se[-1, 1]), lty = 2, lwd = 3)
        }
newmath.wait.for.answer <-
        function (t = list(status = "cancel trial", problem = "5"), measure.travel = FALSE)
        {
                if (t$status != "okay")
                        return(t)
                t1 = t0 = see.time = Sys.time()
                if (measure.travel) {
                        s = "5"
                        while (s == "5") {
                                s = getGraphicsEvent(prompt = "", onKeybd = newmath.get.key)
                                t0 = t1
                                t1 = Sys.time()
                        }
                        t$actual.answer = s
                        t$wait.msec = as.integer(1000 * difftime(t0, see.time,
                                                                 unit = "sec"))
                        t$travel.msec = as.integer(1000 * difftime(t1, t0, unit = "sec"))
                        t$latency.msec = t$wait.msec + t$travel.msec
                }
                else {
                        t$actual.answer = getGraphicsEvent(prompt = "", onKeybd = newmath.get.key)
                        t$latency.msec = as.integer(1000 * difftime(Sys.time(),
                                                                    see.time, unit = "sec"))
                }
                if (t$actual.answer %in% letters)
                        t$actual.answer = LETTERS[t$actual.answer == letters]
                t
        }
newmath.when <-
        function (which = tail(unique(newmath5$condition[newmath5$include])),
                  data = newmath5)
        {
                if (10 == nchar(which[1]))
                        wh = as.POSIXct(which)
                else {
                        wh = newmath.when.one(which[1], data = data)
                        if (length(which) > 1) {
                                for (ind in 2:length(which)) wh[ind] = newmath.when.one(which[ind],
                                                                                        data = data)
                        }
                }
                names(wh) = which
                wh
        }
newmath.when.one <-
        function (cond = tail(newmath5$condition, 1), data = newmath5)
        {
                as.POSIXct(data$when[data$condition == cond])[1]
        }
newmath.wrong <-
        function (which = tail(newmath5$condition, 1), data = newmath5,
                  sef = FALSE)
        {
                if (10 == nchar(which)) {
                        dt = sapply(newmath.conds(which), FUN = newmath.wrong,
                                    sef = TRUE)
                        return(newmath.mean.se(dt["mean", ], zerof = TRUE))
                }
                msk = (which == data$condition) & data$include
                total = sum(msk)
                total.correct = sum(data$correct[msk])
                p = (total - total.correct)/total
                q = 1 - p
                all = 100 * p
                names(all) = "mean"
                if (sef)
                        all = c(all, se = 100 * sqrt(p * q/total))
                all
        }
newmath.wrong.compute <-
        function (msk = newmath5$include, correct = newmath5$correct,
                  sef = FALSE)
        {
                total = sum(msk)
                total.correct = sum(correct[msk])
                p = (total - total.correct)/total
                q = 1 - p
                all = 100 * p
                names(all) = "mean"
                if (sef)
                        all = c(all, se = 100 * sqrt(p * q/total))
                all
        }
newmath.wrong.old <-
        function (data = newmath3, conditions = tail(unique(newmath3$condition[newmath3$include])),
                  sef = FALSE, dayf = FALSE)
        {
                if (dayf) {
                }
                sapply(conditions, newmath.wrong.one, data = data, sef = sef)
        }
newmath.wrong.one <-
        function (data = newmath5, cond = tail(unique(newmath5$condition),
                                               1), sef = FALSE)
        {
                msk = (cond == data$condition) & data$include
                newmath.wrong.compute(msk = msk, correct = data$correct,
                                      sef = sef)
        }
newmath5.add <-
        function (label="",...)
        {#Like newmath2 but with much simpler task: press 8 when you see 8, 4 when you
                #see 4, etc., using only ONE finger instead of 8 fingers (newmath3).
                #
                # 2012-03-31 add correction trials
                #
                newmath5<<-newmath.add(data=newmath5,da.params=newmath5.analysis,label=label,...)
                newmath5.analysis<<-data.analysis
                newmath.save.ws()
                if(label!="warmup")newmath5.plot()
                cat(newmath.menu.prompt)
        }
newmath5.compare.sensitivity <-
        function (conditions=100:160)
        {#compare the sensitivity of different ways of analyzing the data.
                #Returns F values for differences between conditions.
                #
                u=unique(newmath5$condition[newmath5$include])
                cat(length(u),"conditions\n")
                m=newmath5$include & newmath5$correct & newmath5$condition %in% u[conditions]
                c=as.factor(newmath5$condition[m])
                p=as.factor(newmath5$problem[m])
                rt=newmath5$latency.msec[m]
                r=aov(rt~c+p)
                print(summary(r))
                #print(summary(aov(sqrt(rt)~c+p)))
                #print(summary(aov(log(rt)~c+p)))
                #print(summary(aov(1/rt~c+p)))
                #print(summary(aov(newmath5$latency.ptile[m]~c+p)))
                r
        }
newmath5.error.plot <-
        function (group.size=8)
        {#compare errors to actual answer: how far away?
                #
                # group.size number of conditions to include in oneway F test
                #
                save.graphs()
                oldpar=par(las = 1, cex = 1.8)
                ok=newmath5$include&(!newmath5$correct)&(newmath5$actual.answer%in%as.character(2:8))
                ok[is.na(ok)]=F
                diff=as.integer(newmath5$problem[ok])-as.integer(newmath5$actual.answer[ok])
                #hist(diff)
                n=length(diff)
                s=sum(diff==-1)+sum(diff==1)
                cat("total errors",n,round(100*(s/n)),"percent one away from correct\n")
                one.away=(diff==-1)|(diff==1)
                actual=newmath5$actual.answer[ok]
                #hist(as.integer(actual[one.away]))
                ave=matrix(NA,nrow=7,ncol=2,dimnames=list(as.character(2:8),c(T,F)))
                cor=newmath5$include&newmath5$correct
                cor[is.na(cor)]=F
                error.latency=newmath5$latency.msec[ok]
                for (pro in as.character(2:8)) {
                        ave[pro,"TRUE"]=mean(newmath5$latency.msec[cor&newmath5$problem==pro])
                        ave[pro,"FALSE"]=mean(error.latency[actual[one.away]==pro])
                }
                i=newmath5$include
                di=abs(as.integer(newmath5$problem[i])-as.integer(newmath5$actual.answer[i]))
                ok=(di<2)&(newmath5$actual.answer[i]%in%as.character(2:8))
                act=(newmath5$actual.answer[i])[ok]
                cor=as.character((newmath5$correct[i])[ok])
                con=(newmath5$condition[i])[ok]
                adj=lat=(newmath5$latency.msec[i])[ok]
                n=sum(ok)
                for (i in 1:n) {
                        adj[i]=lat[i]-ave[act[i],cor[i]]
                }
                uni=unique(con)
                ncond=length(uni)
                cat(ncond,"conditions\n")
                ntests=ncond%/%group.size
                all.answers=correct.only=numeric(ntests)
                for(test in 1:ntests){
                        cond.nums=((test-1)*group.size)+1:group.size
                        t=oneway.test(adj~con,subset=con%in%uni[cond.nums],var.equal=T)
                        all.answers[test]=t$statistic
                        t=oneway.test(adj~con,subset=(con%in%uni[cond.nums])&(cor=="TRUE"),var.equal=T)
                        correct.only[test]=t$statistic
                }
                x=1:ntests
                plot(x,all.answers,ylim=range(all.answers,correct.only),xlab="Test",ylab="F Value",type="n",log="y")
                points(x,all.answers,pch=16,col="purple")
                lines(lowess(x,all.answers, f = 1/6),lwd=4,col="purple")
                points(x,correct.only,pch=16,col="green")
                lines(lowess(x,correct.only, f = 1/6),lwd=4,col="green")
                legend("bottomright",legend=c("correct only","all answers"),pch=c(16,16),col=c("green","purple"),bty="n")
                rat=all.answers/correct.only
                plot(x,rat,xlab="Test",log="y", ylab="F all answers/F correct only", pch = 16)
                abline(h=1,lty = 2,lwd = 2)
                lines(lowess(x,rat,f = 1/6),lwd = 5,col = "orange")
                highf=correct.only>4
                plot(x[highf],rat[highf],log="y",ylab="F all answers/F correct only",pch=16)
                lines(lowess(x[highf],rat[highf],f = 1/6),lwd = 5,col = "orange")
                abline(h=1,lty = 2,lwd = 2)
        }
newmath5.export <-
        function ()
        {#put the newmath functions and data in the same location that put uses.
                #When the newmath version changes (e.g., from newmath3 to newmath5,
                #this function must be changed.
                #
                temp.newmath5<<-newmath5
                newmath5<<-tail(newmath5[newmath5$include,],2)
                newmath5$include<<-FALSE
                #
                temp.newmath.save.ws<<-newmath.save.ws
                newmath.save.ws<<-newmath.export.save.ws
                run.warmup<<-newmath5.warmup
                run.test<<-newmath5.add
                see.results<<-newmath5.plot
                me<<-newmath.me
                .First<<-newmath.First
                #
                all=ls(name=as.environment(1))
                part1=grep(pattern="newmath.",x=all,fixed=TRUE)
                part2=grep(pattern="newmath5",x=all)
                part3=c("run.warmup","run.test","see.results","me",".First")
                save(list=c(all[part1],all[part2], part3),file="C:\\Users\\Seth\\Dropbox\\braintest.RData")
                #
                newmath5<<-temp.newmath5
                newmath.save.ws<<-temp.newmath.save.ws
                rm(list=c("temp.newmath5","temp.newmath.save.ws"),inherits=TRUE)
                rm(list=c("run.warmup","run.test","see.results",".First","me"),inherits=TRUE)
        }
newmath5.export.check <-
        function ()
        {
                if (newmath.export.data$preload.removed)
                        return()
                cutoff = newmath.export.data$preload.trials + newmath.export.data$remove.preload.after.trials
                if (cutoff < nrow(newmath5[newmath5$include, ])) {
                        newmath5 <<- tail(newmath5, -newmath.export.data$preload.trials)
                        newmath.export.data$preload.removed <<- TRUE
                        cat("preloaded data removed\n")
                }
        }
newmath5.plot <-
        function ()
        {newmath.save.graphs()
                newmath.plot(data=newmath5,last.cond=1000, recentf=FALSE, distributionf=FALSE)
                newmath.plot(data=newmath5,last.cond=100, recentf=FALSE, distributionf=FALSE)
                newmath.plot(data=newmath5,last.cond=30, recentf=FALSE, distributionf=FALSE)
                newmath.plot(data=newmath5,last.cond=20, recentf=FALSE, distributionf=FALSE)
                t=newmath.plot(data=newmath5,last.cond=100)
                print(t)
                newmath.compare.all(data=newmath5)
                cat(newmath.menu.prompt)
                invisible(t)
        }
newmath5.sensitivity <-
        function (conditions = 20:140)
        {
                newmath5$condition = as.factor(newmath5$condition)
                u = (unique(newmath5$condition[newmath5$include]))[conditions]
                m = newmath5$include & (newmath5$condition %in% u)
                fvalues = NULL
                prblms = sort(unique(newmath5$problem))
                for (p in prblms) {
                        t = lm(latency.ptile ~ condition, data = newmath5, subset = m &
                                       (newmath5$problem == p))
                        t1 = summary(t)
                        t2 = t1$fstatistic
                        fvalues = c(fvalues, t2["value"])
                }
                plot(as.integer(prblms), fvalues, ylab = "F value", xlab = "Problem",
                     pch = 16, cex = 1.8, col = "red")
        }
newmath5.warmup <-
        function (trials = 6, note = "warmup")
        {
                newmath5.add(trials = trials, label = "warmup", note = note)
        }
newmath56.export <-
        function (previous.data = newmath.export.data$preload.trials)
        {
                temp.newmath6 <<- newmath6
                temp.newmath5 <<- newmath5
                newmath5 <<- tail(newmath5[newmath5$include, ], previous.data)
                newmath6 <<- tail(newmath6[newmath6$include, ], previous.data)
                temp.newmath.save.ws <<- newmath.save.ws
                newmath.save.ws <<- newmath.export.save.ws
                newmath.export.data$preload.removed <<- FALSE
                all = ls(name = as.environment(1))
                part1 = grep(pattern = "newmath.", x = all, fixed = TRUE)
                part2 = grep(pattern = "newmath6", x = all)
                part3 = grep(pattern = "newmath5", x = all)
                save(list = c(all[part1], all[part2], all[part3]), file = "C:\\Users\\Seth\\Documents\\newmath56.RData")
                cat("saved C:\\Users\\Seth\\Documents\\newmath56.RData\n")
                newmath6 <<- temp.newmath6
                newmath5 <<- temp.newmath5
                newmath.save.ws <<- temp.newmath.save.ws
                rm(list = c("temp.newmath6", "temp.newmath5", "temp.newmath.save.ws"),
                   inherits = TRUE)
                newmath.export.data$preload.removed <<- TRUE
        }
newmath5vs6 <-
        function ()
        {
                if (1 == sample(2, size = 1)) {
                        newmath5.add(label = "newmath5 first")
                        newmath6.add(label = "newmath5 first")
                }
                else {
                        newmath6.add(label = "newmath6 first")
                        newmath5.add(label = "newmath6 first")
                }
                "test finished"
        }
newmath5vs6.compare <-
        function (samples = 400, tests = 3, drop = 0, head.num = 100)
        {
                save.graphs()
                ok5 = substr(newmath5$condition, 18, 31) %in% c("newmath5 first",
                                                                "newmath6 first")
                ok5 = ok5 & newmath5$correct & newmath5$include
                ok6 = substr(newmath6$condition, 18, 31) %in% c("newmath5 first",
                                                                "newmath6 first")
                ok6 = ok6 & newmath6$correct & newmath6$include
                a = newmath5$latency.msec[ok5]
                a.name = "5 msec"
                acond = newmath5$condition[ok5]
                aprob = newmath5$problem[ok5]
                b = newmath6$latency.msec[ok6] + newmath6$travel.msec[ok6]
                b.name = "6 lat + trav"
                bcond = newmath6$condition[ok6]
                bprob = newmath6$problem[ok6]
                if (drop > 0) {
                        conds5 = head(tail(unique(newmath5$condition[ok5]), -drop),
                                      head.num)
                        conds6 = head(tail(unique(newmath6$condition[ok6]), -drop),
                                      head.num)
                }
                else {
                        conds5 = head(unique(newmath5$condition[ok5]), head.num)
                        conds6 = head(unique(newmath6$condition[ok6]), head.num)
                }
                if (!length(conds5) == length(conds6))
                        return("number of conditions doesn't match")
                n = length(conds5)
                cat(n, "comparisons\n")
                fmatrix = matrix(NA, ncol = 2, nrow = samples, dimnames = list(NULL,
                                                                               c(a.name, b.name)))
                for (s in 1:samples) {
                        sam = sample(n, tests)
                        ok51 = acond %in% conds5[sam]
                        ok61 = bcond %in% conds6[sam]
                        fmatrix[s, a.name] = newmath5vs6.compare.fvalue(a[ok51],
                                                                        acond[ok51], aprob[ok51])
                        fmatrix[s, b.name] = newmath5vs6.compare.fvalue(b[ok61],
                                                                        bcond[ok61], bprob[ok61])
                }
                print(colMeans(fmatrix))
                d = log(fmatrix[, 1]) - log(fmatrix[, 2])
                mean.se(d)
        }
newmath5vs6.compare.fvalue <-
        function (value, condition, problem)
        {
                t = lm(value ~ as.factor(condition) + as.factor(problem))
                t1 = summary(aov(t))
                (t1[[1]])[1, 4]
        }
newmath5vs6.condition <-
        function ()
        {
        }
newmath5vs6.conditions <-
        function (drop = 0, head.num = 100)
        {
                ok5 = substr(newmath5$condition, 18, 31) %in% c("newmath5 first",
                                                                "newmath6 first")
                ok5 = ok5 & newmath5$correct & newmath5$include
                ok6 = substr(newmath6$condition, 18, 31) %in% c("newmath5 first",
                                                                "newmath6 first")
                ok6 = ok6 & newmath6$correct & newmath6$include
                a = newmath5$latency.msec[ok5]
                a.name = "5 msec"
                acond = newmath5$condition[ok5]
                aprob = newmath5$problem[ok5]
                b = newmath6$latency.msec[ok6] + newmath6$travel.msec[ok6]
                b.name = "6 lat + trav"
                bcond = newmath6$condition[ok6]
                bprob = newmath6$problem[ok6]
                if (drop > 0) {
                        conds5 = head(tail(unique(newmath5$condition[ok5]), -drop),
                                      head.num)
                        conds6 = head(tail(unique(newmath6$condition[ok6]), -drop),
                                      head.num)
                }
                else {
                        conds5 = head(unique(newmath5$condition[ok5]), head.num)
                        conds6 = head(unique(newmath6$condition[ok6]), head.num)
                }
                cat(length(conds5), "conditions\n")
                invisible(list(conds5 = conds5, conds6 = conds6))
        }
newmath5vs6.fvalue <-
        function (data = newmath5, conds = tail(unique(newmath5$condition[newmath5$include &
                                                                                  newmath5$correct]), 3), ok = newmath5$include & newmath5$correct)
        {
                cond.mask = data$condition %in% conds
                ok1 = ok & cond.mask
                val = data$latency.msec[ok1]
                con = as.factor(data$condition[ok1])
                pro = as.factor(data$problem[ok1])
                t = lm(val ~ con + pro)
                t1 = summary(aov(t))
                (t1[[1]])[1, 4]
        }
newmath5vs6.omit.one <-
        function (samples = 10000, pairs = 4, drop = 0, head.num = 100)
        {
                start = Sys.time()
                c = newmath5vs6.conditions(head.num = head.num, drop = drop)
                c5 = c$conds5
                c6 = c$conds6
                cat("computing all conditions together\n")
                omit.one = newmath5vs6.samples(conds5 = c5, conds6 = c6,
                                               pairs = pairs, samples = 10 * samples)
                cat("computing results with one condition omitted\n")
                nv = 1:(length(c5))
                print(nv)
                for (om in nv) {
                        cat("omit pair", om, "\n")
                        c5m = c5[-om]
                        c6m = c6[-om]
                        omit.one = rbind(omit.one, newmath5vs6.samples(conds5 = c5m,
                                                                       conds6 = c6m, pairs = pairs, samples = samples))
                }
                names(omit.one) = paste("omit pair", nv)
                duration = difftime(Sys.time(), start)
                newmath5vs6.omit.one.data <<- list(all = omit.one[1, ], omit.one = omit.one[-1,
                                                                                            ], pairs = pairs, samples = samples, conds5 = c$conds5,
                                                   conds6 = c$conds6, duration = duration)
                cat("set newmath5vs6.omit.one.data\n")
                save.ws()
        }
newmath5vs6.omit.one.plot <-
        function (cv = "red")
        {
                oldpar = par(las = 1, cex = 2)
                on.exit(par(oldpar))
                t = newmath5vs6.omit.one.data
                sev = sqrt(t$all[2]^2 + t$omit.one[, 2]^2)
                adv = t$all[1] - t$omit.one[, 1]
                top = adv + sev
                bot = adv - sev
                xv = 1:nrow(t$omit.one)
                plot(xv, adv, ylim = range(top, bot), xlab = "Pair", ylab = "Newmath5 Advantage (log)",
                     col = cv, pch = 16, cex = 1.2)
                segments(xv, top, xv, bot, lwd = 4)
                abline(h = 0, lwd = 4, lty = 2)
                print(t.test(adv))
        }
newmath5vs6.samples <-
        function (conds5, conds6, samples = 400, pairs = 3)
        {
                nm5ok = newmath5$include & newmath5$correct
                nm6ok = newmath6$include & newmath6$correct
                fmatrix = matrix(NA, ncol = 2, nrow = samples, dimnames = list(NULL,
                                                                               c("newmath5", "newmath6")))
                for (s in 1:samples) {
                        n = length(conds5)
                        sam = sample(n, pairs)
                        fmatrix[s, "newmath5"] = newmath5vs6.fvalue(data = newmath5,
                                                                    ok = nm5ok, conds = conds5[sam])
                        fmatrix[s, "newmath6"] = newmath5vs6.fvalue(data = newmath6,
                                                                    ok = nm6ok, conds = conds6[sam])
                }
                d = log(fmatrix[, "newmath5"]) - log(fmatrix[, "newmath6"])
                mean.se(d)
        }
newmath5vs7 <-
        function ()
        {
                if (1 == sample(2, size = 1)) {
                        newmath5.add(label = "nm5 1st nm7 2nd")
                        newmath7.add(label = "nm5 1st nm7 2nd")
                }
                else {
                        newmath7.add(label = "nm7 1st nm5 2nd")
                        newmath5.add(label = "nm7 1st nm5 2nd")
                }
                u = unique(newmath7$condition[newmath7$include & newmath7$correct])
                cat(sum(substr(u, 18, 32) %in% c("nm7 1st nm5 2nd", "nm5 1st nm7 2nd")),
                    "comparisons\n")
                "test finished"
        }
newmath5vs8 <-
        function ()
        {
                if (1 == sample(2, size = 1)) {
                        newmath5.add(label = "nm5 1st nm8 2nd")
                        newmath8.add(label = "nm5 1st nm8 2nd")
                }
                else {
                        newmath8.add(label = "nm8 1st nm5 2nd")
                        newmath5.add(label = "nm8 1st nm5 2nd")
                }
                u = unique(newmath8$condition[newmath8$include & newmath8$correct])
                cat(sum(substr(u, 18, 32) %in% c("nm8 1st nm5 2nd", "nm5 1st nm8 2nd")),
                    "comparisons\n")
                "test finished"
        }
put <-
        function (..., file = "C:\\temp0.RData")
        {
                save(..., file = file)
        }
run.test <-
        function (label="",...)
        {#Like newmath2 but with much simpler task: press 8 when you see 8, 4 when you
                #see 4, etc., using only ONE finger instead of 8 fingers (newmath3).
                #
                # 2012-03-31 add correction trials
                #
               # X11(type="Xlib")
                newmath5<<-newmath.add(data=newmath5,da.params=newmath5.analysis,label=label,...)
                newmath5.analysis<<-data.analysis
                newmath.save.ws()
                if(label!="warmup")newmath5.plot()
                cat(newmath.menu.prompt)
        }
run.warmup <-
        function (trials=6,note="warmup")
        {# warmup trials.
                #
                #  trials   number of trials
                #
               # X11(type = "Xlib")
                newmath5.add(trials = trials, label="warmup",note=note)
        }
save.graphs <-
        function ()
        {
        }
save.ws <-
        function (nm = "percentilefeedback")
        {
                invisible()
                .SavedPlots = NULL
                lo = paste("C:/", nm, ".RData", sep = "")
                save.image(lo)
                cat(nm, "workspace saved at", lo, "\n")
                cat(as.character(Sys.time()), "\n")
        }
see.results <-
        function ()
        {
                #quartz()   
                newmath.save.graphs()
                newmath.plot(data=newmath5,last.cond=1000, recentf=FALSE, distributionf=FALSE)
                newmath.plot(data=newmath5,last.cond=100, recentf=FALSE, distributionf=FALSE)
                newmath.plot(data=newmath5,last.cond=30, recentf=FALSE, distributionf=FALSE)
                newmath.plot(data=newmath5,last.cond=20, recentf=FALSE, distributionf=FALSE)
                t=newmath.plot(data=newmath5,last.cond=100)
                print(t)
                newmath.compare.all(data=newmath5)
                cat(newmath.menu.prompt)
                invisible(t)
        }
take <-
        function ()
        {
                load(file = "C:\\temp0.RData", .GlobalEnv)
        }
temp.newmath.save.ws <-
        function (nm="newtracking", directory=getwd())
        {
                invisible()
                .SavedPlots=NULL
                lo=paste(directory,"/",nm,".RData",sep="")
                save.image(lo)
                cat("workspace saved at",lo,"\n")
                cat(as.character(Sys.time()),"\n")
                backup.ws(nm=nm,directory=directory,addname="onehourbackup",hrs=1)
                backup.ws(nm=nm,directory=directory,addname="threehourbackup",hrs=3)
                backup.ws(nm=nm,directory=directory,addname="ninehourbackup",hrs=9)
                backup.ws(nm=nm,directory=directory,addname="onedaybackup",hrs=24)
                backup.ws(nm=nm,directory=directory,addname="threedaybackup",hrs=3*24)
                backup.ws(nm=nm,directory=directory,addname="ninedaybackup",hrs=9*24)
        }
work.bonus <-
        function (correct.bonus)
        {
                old.bonus = work[nrow(work), "bonus"]
                work[nrow(work), "bonus"] <<- correct.bonus
                cat("bonus was", old.bonus, "now", correct.bonus, "\n")
                work.so.far()
                save.ws()
        }
work.dots <-
        function (correct.dots = TRUE)
        {
                old.dots = work[nrow(work), "dots"]
                work[nrow(work), "dots"] <<- correct.dots
                cat("dots was", old.dots, "now", correct.dots, "\n")
                work.so.far()
                save.ws()
        }
work.duration <-
        function (dura)
        {
                nr = nrow(work)
                old = work[nr, "duration"]
                work[nr, "duration"] <<- dura
                cat("duration of", work[nr, "task"], "was", old, "now", dura,
                    "minutes\n")
                save.ws()
        }
work.duration.so.far <-
        function (then = work[nrow(work), c("hour.start", "minute.start",
                                            "second.start")])
        {
                di = current()[4:6] - then
                c(60, 1, (1/60)) %*% as.numeric(di)
        }
work.erase <-
        function ()
        {
                work <<- work[-nrow(work), ]
                cat("last row of work erased\n")
        }
work.hrs.off <-
        function (hrs.off)
        {
                work[nrow(work), "hrs.off"] <<- hrs.off
                cat("hrs.off set\n")
                save.ws()
        }
work.instructions <-
        function ()
        {
        }
work.last <-
        function ()
        {
                tail(unique(rev(tail(work$task, 15))), 8)
        }
work.note <-
        function (note)
        {
                nr = nrow(work)
                old = work[nr, "note"]
                work[nr, "note"] <<- note
                cat("note for", work[nr, "task"], "was", old, "now", note,
                    "\n")
                save.ws()
        }
work.plot <-
        function (first.hr = 3, dayf = FALSE)
        {
                save.graphs()
                work.so.far()
                all = work.plot.compute(first.hr = first.hr)
                ptile = work.plot.compare(all)
                work.plot.hour(all, ptile = ptile)
                if (dayf)
                        work.plot.day(all)
        }
work.plot.compare <-
        function (all = work.plot.compute(), spread = 1)
        {
                last.time = tail(all[, "hour"], 1)
                last.eff = tail(all[, "efficiency"], 1)
                later.msk = all[, "hour"] > (last.time - spread)
                earlier.msk = all[, "hour"] < (last.time + spread)
                sam = all[(later.msk & earlier.msk), "efficiency"]
                work.plot.ptile(last.eff, sam[!is.na(sam)])
        }
work.plot.compute <-
        function (first.hr = 3, fd.yr = 2011, fd.mo = 3, fd.da = 27)
        {
                day.id = (31 * work$month) + work$day
                feedback.id = (31 * fd.mo) + fd.da
                feed.num = as.integer(feedback.id <= day.id)
                u = unique(day.id[work$include])
                feed.num = as.integer(feedback.id <= u)
                all = matrix(NA, ncol = 7, nrow = 1)
                dimnames(all) = list(NULL, c("year", "month", "day", "hour",
                                             "efficiency", "final", "feedback"))
                for (i in 1:length(u)) {
                        msk = u[i] == day.id
                        yr = (work$year[msk])[1]
                        mo = (work$month[msk])[1]
                        da = (work$day[msk])[1]
                        feedf = feed.num[i]
                        all = rbind(all, work.plot.compute.one(yr, mo, da, first.hr = first.hr,
                                                               today = i == length(u), feedback = feedf))
                }
                all[-1, ]
        }
work.plot.compute.one <-
        function (year = 2011, month = 4, day = 8, first.hr = 3, today = FALSE,
                  feedback = 0)
        {
                day.msk = (work$year == year) & (work$month == month) & (work$day ==
                                                                                 day)
                hr = work$hour.start[day.msk] + (work$minute.start[day.msk]/60) +
                        work$second.start[day.msk]/3600
                o = order(hr)
                du = ((work$duration[day.msk])[o])/60
                hr = hr[o]
                m = matrix(c(hr[1], 0), ncol = 2, dimnames = list(NULL, c("hour",
                                                                          "work.hr")))
                m = rbind(m, c(hr[1] + du[1], du[1]))
                n = length(hr)
                if (n > 1) {
                        for (i in 2:n) {
                                nr = nrow(m)
                                m = rbind(m, c(hr[i], m[nr, "work.hr"]))
                                m = rbind(m, c(hr[i] + du[i], m[nr, "work.hr"] +
                                                       du[i]))
                        }
                }
                if (today) {
                        nr = nrow(m)
                        now = current(hour = TRUE)
                        if (is.na(m[nr, "hour"])) {
                                m[nr, "hour"] = now
                                m[nr, "work.hr"] = m[nr - 1, "work.hr"] + work.duration.so.far()/60
                        }
                        else m = rbind(m, c(now, m[nr, "work.hr"]))
                }
                sl = work.sleep(year, month, day)
                fh = max(first.hr, sl[1])
                if (is.na(sl[2]))
                        avail = m[, "hour"] - fh
                else avail = m[, "hour"] - (fh + (m[, "hour"] > sl[2]) *
                                                    sl[3])
                eff = 100 * m[, "work.hr"]/avail
                eff[eff > 100] = 100
                n = length(eff)
                finalf = rep(0:1, times = c(n - 1, 1))
                out = cbind(year, month, day, m[, "hour"], eff, finalf, feedback)
                dimnames(out) = list(NULL, c("year", "month", "day", "hour",
                                             "efficiency", "final", "feedback"))
                out
        }
work.plot.day <-
        function (all = work.plot.compute(), cv = c("red", "black"),
                  now.cut = TRUE)
        {
                oldpar = par(las = 1, cex = 1.8, mgp = c(2.4, 0.7, 0), mar = c(6,
                                                                               5, 3, 5))
                on.exit(par(oldpar))
                last.mask = as.logical(all[, "final"])
                last.mask[is.na(last.mask)] = FALSE
                y = all[last.mask, "year"]
                m = all[last.mask, "month"]
                d = all[last.mask, "day"]
                feedf = head(all[last.mask, "feedback"], -now.cut)
                dv = head(as.Date(paste(y, m, d), format = "%Y %m %d"), -now.cut)
                eff = head(all[last.mask, "efficiency"], -now.cut)
                plot(dv, eff, xlab = "Date", ylab = "Efficiency (percent)",
                     pch = 16, type = "n")
                points(dv, eff, pch = 16, cex = 1.6, col = cv[1 + feedf])
                lines(loess.smooth(dv[feedf == 0], eff[feedf == 0], span = 100,
                                   degree = 1), lwd = 7, col = cv[1])
                lines(loess.smooth(dv[feedf == 1], eff[feedf == 1], span = 100,
                                   degree = 1), lwd = 7, col = cv[2])
                title("Efficiency vs. Day", line = 0.7)
                lg = c("no percentile feedback", "percentile feedback")
                legend("topleft", pch = 16, col = cv, legend = lg, bty = "n")
        }
work.plot.hour <-
        function (all = work.plot.compute(), first.hr = 3, last.hr = 23.5,
                  legendf = FALSE, ptile = -1)
        {
                today.color = "brown"
                cm = matrix(c("blue", "green", "red", "black"), nrow = 2,
                            ncol = 2)
                dimnames(cm) = list(c("baseline", "feedback"), c("non-final",
                                                                 "final"))
                hr.range = c(first.hr, last.hr)
                oldpar = par(las = 1, cex = 1.8, mgp = c(2, 0.5, 0))
                on.exit(par(oldpar))
                day.id = (31 * all[, "month"]) + all[, "day"]
                u = unique(day.id)
                n = length(u)
                plot(first.hr:last.hr, ylim = c(0, 100), ylab = "Efficiency (percent)",
                     xlab = "Time of Day (hr)", xlim = hr.range, type = "n")
                for (i in 1:n) {
                        all.part = all[u[i] == day.id, ]
                        if (all.part[1, "feedback"] < 1)
                                cv = cm["baseline", ]
                        else cv = cm["feedback", ]
                        work.plot.hour.one(all.part, today = (i == n), first.hr = first.hr,
                                           cv = cv, today.color = today.color)
                }
                lt = c("percentile feedback (final)", "percentile feedback (non-final)",
                       "no percentile feedback (final)", "no percentile feedback (non-final)",
                       "today")
                if (legendf)
                        legend("topright", legend = lt, col = cv, pch = 16, bty = "n",
                               cex = 0.8)
                else mtext(ptile, adj = 0.97, padj = 1, cex = 4, line = -1)
                title("Efficiency (time spent working/available time) vs Time of Day",
                      line = 0.7)
        }
work.plot.hour.one <-
        function (all.part, first.hr = 3, today = FALSE, cv = rainbow(2),
                  today.color = "brown")
        {
                if (today)
                        lines(all.part[, "hour"], all.part[, "efficiency"], lwd = 5,
                              col = today.color)
                else points(all.part[, "hour"], all.part[, "efficiency"],
                            pch = 16, cex = 1, col = rep(cv, times = c(nrow(all.part) -
                                                                               1, 1)))
        }
work.plot.ptile <-
        function (x, samp)
        {
                if (x == max(samp))
                        return(100)
                low = max(samp[x > samp])
                high = min(samp[x < samp])
                n = length(samp)
                low.qtile = sum(low >= samp)/n
                high.qtile = sum(high >= samp)/n
                frac = (x - low)/(high - low)
                round(100 * (low.qtile + frac * (high.qtile - low.qtile)))
        }
work.recent <-
        function ()
        {work.last()
        }
work.restart <-
        function (...)
        {
                work <<- work[-nrow(work), ]
                work.start(...)
        }
work.set.types <-
        function ()
        {
                work$year <<- as.integer(work$year)
                work$month <<- as.integer(work$month)
                work$day <<- as.integer(work$day)
                work$hour.start <<- as.integer(work$hour.start)
                work$minute.start <<- as.integer(work$minute.start)
                work$second.start <<- as.integer(work$second.start)
                work$task <<- as.character(work$task)
                work$eagerness <<- as.integer(work$eagerness)
                work$bonus <<- as.integer(work$bonus)
                work$goal <<- as.integer(work$goal)
                work$dots <<- as.logical(work$dots)
                work$duration <<- as.numeric(work$duration)
                work$hrs.off <<- as.numeric(work$hrs.off)
                work$include <<- as.logical(work$include)
                work$note <<- as.character(work$note)
                row.names(work) <<- NULL
        }
work.sleep <-
        function (year = 2011, month = 3, day = 29, default = 3)
        {
                target.date = (as.Date(paste(year, month, day), format = "%Y %m %d")) -
                        1
                dys = as.Date(paste(sleep$year, sleep$month, sleep$day),
                              format = "%Y %m %d")
                t = sleep[dys == target.date, c("time.up", "time.up.2", "min.slept.2")]
                if (nrow(t[1]) < 1)
                        return(c(default, NA, NA))
                t[1:2] = t[1:2]%/%100 + (t[1:2]%%100)/60
                if (!is.na(t[1])) {
                        if (t[1] > 10)
                                t[1] = t[1] - 12
                }
                t[3] = t[3]/60
                as.numeric(t)
        }
work.so.far <-
        function (thisf = TRUE, datef = TRUE)
        {
                du = work.duration.so.far()
                msk = (current.year() == work$year) & (current.month() ==
                                                               work$month) & (current.day() == work$day)
                all = sum(work$duration[msk], na.rm = TRUE) + sum(work$bonus[msk])
                working.now = is.na(tail(work$duration, 1))
                if (working.now) {
                        if (thisf) {
                                if (1 == round(du))
                                        cat("1 minute of", tail(work$task, 1), "\n")
                                else cat(round(du), "minutes of", tail(work$task,
                                                                       1), "\n")
                        }
                        all = all + du
                }
                cat(round(all), "minutes today\n")
                if (datef)
                        cat(as.character(Sys.time()), "\n")
                invisible(du)
        }
work.start <-
        function (task=1,hour,minute,eagerness=NA,bonus=0,offset=0,goal=NA,hrs.off=0,dots=FALSE,location="apartment",include=TRUE,note="")
        {#Track work data.
                #
                #    task       task starting (integers refer to items on list of recent
                #               tasks, with 1 = most recent, 2 = next most recent, etc.)
                #    hour       when started
                #    minute       
                #    bonus      bonus minutes
                #    offset     decrease starting time by this amount (minutes)
                #    goal       goal (minutes)
                #    hrs.off    hours spent doing something unusual
                #    dots       used dots to measure progress
                #    location   location of work
                #    include    include this bout?
                #
                last.tasks=work.last()
                if(is.numeric(task)) task=last.tasks[as.integer(task)]
                cu=current()
                if(!missing(hour)){
                        cu["hour"]=hour
                        cu["minute"]=minute
                }
                new.line=c(cu,task,duration=NA,eagerness=eagerness,bonus=bonus,goal=goal,dots,location,hrs.off,include,note)
                new.line["minute"]=as.integer(new.line["minute"])-offset
                while(as.integer(new.line["minute"])<0){
                        new.line["hour"]=as.integer(new.line["hour"])-1
                        new.line["minute"]=60+as.integer(new.line["minute"])
                }
                br=work.duration.so.far()-tail(work$duration,1)
                if(is.na(br)) return("need to fill in most recent duration")
                if (br<0) br=br+(24*60)
                cat(round(br),"minutes break\n")
                work<<-rbind(work,new.line)
                work.set.types()
                cat(task,"started at",substr(Sys.time(),12,16),"\n")
                work.so.far(thisf=FALSE)
                save.ws()
                work.plot()
        }
work.stop <-
        function (duration, note, bonus, offset = 0, add = 0)
        {
                nr = nrow(work)
                if (!missing(note))
                        work[nr, "note"] <<- note
                if (!missing(bonus))
                        work[nr, "bonus"] <<- bonus
                if (missing(duration))
                        duration <- work.duration.so.far() + (add - offset)
                cat(work[nr, "task"], "stopped\n")
                cat("lasted", round(duration), "minutes\n")
                work[nr, "duration"] <<- duration
                work.so.far(thisf = FALSE)
                work.set.types()
                save.ws()
                work.plot()
        }
work.switch <-
        function (new.task)
        {
                work.stop()
                work.start(task = new.task)
        }
work.task <-
        function (correct.task)
        {
                old.task = work[nrow(work), "task"]
                work[nrow(work), "task"] <<- correct.task
                cat("task was", old.task, "now", correct.task, "\n")
                save.ws()
        }
work.version <-
        function ()
        {
        }
work.whole <-
        function (task, hour, minute = 0, duration, eagerness = NA, include = TRUE,
                  bonus = 0, goal = NA, hrs.off = 0, dots = FALSE, location = "apartment",
                  note = "")
        {
                if (missing(task))
                        task = last(work$task)
                new.line = c(current(), task, duration, eagerness, bonus,
                             goal, dots, location, hrs.off, include, note)
                if (!missing(hour))
                        new.line["hour"] = as.integer(hour)
                if (!missing(minute))
                        new.line["minute"] = as.integer(minute)
                work <<- rbind(work, new.line)
                work.set.types()
                work.so.far(thisf = FALSE, datef = FALSE)
                save.ws()
        }
