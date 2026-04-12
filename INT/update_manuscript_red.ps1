$ErrorActionPreference = 'Stop'

$wdColorRed = 255
$wdStyleNormal = -1
$wdStyleHeading2 = -3
$wdAlignParagraphCenter = 1
$wdBorderTop = -1
$wdBorderLeft = -2
$wdBorderBottom = -3
$wdBorderRight = -4
$wdBorderHorizontal = -5
$wdBorderVertical = -6
$wdLineStyleNone = 0
$wdLineStyleSingle = 1
$wdLineSpaceSingle = 0

Set-Location -LiteralPath $PSScriptRoot
$outputName = 'manuscript_' + [string]([char]0x526F) + [string]([char]0x672C) + '.docx'
$workingName = 'manuscript_copy_red.docx'
try {
    Copy-Item -LiteralPath 'manuscript.docx' -Destination $workingName -Force
}
catch {
    if (Test-Path -LiteralPath $outputName) {
        Copy-Item -LiteralPath $outputName -Destination $workingName -Force
    }
    elseif (-not (Test-Path -LiteralPath $workingName)) {
        throw
    }
}

$baseline = Import-Csv -LiteralPath 'anova_outputs\03_baseline_characteristics.csv'
$main = Import-Csv -LiteralPath 'anova_outputs\13_main_result_table_full.csv'
$table4Display = Import-Csv -LiteralPath 'anova_outputs\16_table4_lower_limb_display.csv'
$table5Display = Import-Csv -LiteralPath 'anova_outputs\17_table5_start_kinematic_display.csv'
$table6Display = Import-Csv -LiteralPath 'anova_outputs\18_table6_fms_display.csv'

$word = New-Object -ComObject Word.Application
$word.Visible = $false
$word.DisplayAlerts = 0
$doc = $word.Documents.Open((Resolve-Path $workingName).Path)
$sel = $word.Selection

function Get-ParaText($para) {
    return (($para.Range.Text -replace "[`r`a]", '') -replace '\s+', ' ').Trim()
}

function Find-ParaIndex([string]$prefix) {
    for ($i = 1; $i -le $doc.Paragraphs.Count; $i++) {
        $txt = Get-ParaText $doc.Paragraphs.Item($i)
        if ($txt.StartsWith($prefix)) { return [int]$i }
    }
    throw "Paragraph starting with '$prefix' not found."
}

function Replace-ParagraphText([string]$prefix, [string]$newText) {
    $idx = Find-ParaIndex $prefix
    $para = $doc.Paragraphs.Item($idx)
    $style = $para.Range.Style
    $para.Range.Text = $newText + "`r"
    $para.Range.Style = $style
    $doc.Range($para.Range.Start, $para.Range.End - 1).Font.Color = $wdColorRed
}

function Replace-SubstringInParagraph([string]$prefix, [string]$old, [string]$new) {
    $idx = Find-ParaIndex $prefix
    $para = $doc.Paragraphs.Item($idx)
    $style = $para.Range.Style
    $text = Get-ParaText $para
    $updated = $text.Replace($old, $new)
    $para.Range.Text = $updated + "`r"
    $para.Range.Style = $style
    $doc.Range($para.Range.Start, $para.Range.End - 1).Font.Color = $wdColorRed
}

function Delete-RangeBetweenHeadings([string]$startHeading, [string]$endHeading) {
    $startIdx = Find-ParaIndex $startHeading
    $endIdx = Find-ParaIndex $endHeading
    $startPos = $doc.Paragraphs.Item($startIdx + 1).Range.Start
    $endPos = $doc.Paragraphs.Item($endIdx).Range.Start
    $range = $doc.Range($startPos, $endPos)
    $insertPos = [int]$range.Start
    $range.Delete() | Out-Null
    return [int]$insertPos
}

function Add-RedParagraph([string]$text, [int]$style = $wdStyleNormal, [int]$align = 0) {
    $sel.Style = $style
    $sel.Font.Color = $wdColorRed
    $sel.ParagraphFormat.Alignment = $align
    $sel.ParagraphFormat.LineSpacingRule = $wdLineSpaceSingle
    $sel.TypeText($text)
    $sel.TypeParagraph()
}

function Add-RedTable([object[][]]$rows) {
    $rowCount = $rows.Count
    $colCount = $rows[0].Count
    $table = $doc.Tables.Add($sel.Range, $rowCount, $colCount)
    $table.Range.Font.Color = $wdColorRed
    $table.Range.Font.Name = 'Times New Roman'
    for ($r = 1; $r -le $rowCount; $r++) {
        for ($c = 1; $c -le $colCount; $c++) {
            $table.Cell($r, $c).Range.Text = [string]$rows[$r - 1][$c - 1]
        }
    }
    $table.Rows.Item(1).Range.Bold = $true
    $table.Borders.Enable = 0
    foreach ($borderId in @($wdBorderTop, $wdBorderLeft, $wdBorderBottom, $wdBorderRight, $wdBorderHorizontal, $wdBorderVertical)) {
        $table.Borders.Item($borderId).LineStyle = $wdLineStyleNone
    }
    $table.Rows.Item(1).Borders.Item($wdBorderTop).LineStyle = $wdLineStyleSingle
    $table.Rows.Item(1).Borders.Item($wdBorderBottom).LineStyle = $wdLineStyleSingle
    $table.Rows.Item($rowCount).Borders.Item($wdBorderBottom).LineStyle = $wdLineStyleSingle
    for ($r = 2; $r -le $rowCount; $r++) {
        $rowValues = $rows[$r - 1]
        $isSectionRow = $true
        for ($c = 2; $c -le $colCount; $c++) {
            if ([string]::IsNullOrWhiteSpace([string]$rowValues[$c - 1]) -eq $false) {
                $isSectionRow = $false
                break
            }
        }
        if ($isSectionRow) {
            $table.Cell($r, 1).Merge($table.Cell($r, $colCount))
            $table.Cell($r, 1).Range.Bold = $true
            $table.Cell($r, 1).Range.ParagraphFormat.Alignment = 0
        }
    }
    $table.AutoFitBehavior(1) | Out-Null
    $sel.SetRange([int]$table.Range.End, [int]$table.Range.End)
    $sel.TypeParagraph()
}

function Add-CenteredPicture([string]$path, [double]$widthPoints) {
    $sel.ParagraphFormat.Alignment = $wdAlignParagraphCenter
    $shape = $sel.InlineShapes.AddPicture((Resolve-Path $path).Path)
    $shape.LockAspectRatio = -1
    $shape.Width = $widthPoints
    $shape.Range.ParagraphFormat.Alignment = $wdAlignParagraphCenter
    $sel.SetRange([int]$shape.Range.End, [int]$shape.Range.End)
    $sel.TypeParagraph()
    $sel.ParagraphFormat.Alignment = 0
}

function Main-Row([string]$outcome) {
    return $main | Where-Object { $_.outcome -eq $outcome } | Select-Object -First 1
}

function FmtNum([object]$value, [int]$digits = 2) {
    if ($null -eq $value -or $value -eq '') { return 'NA' }
    $num = [double]$value
    return [string]::Format([System.Globalization.CultureInfo]::InvariantCulture, "{0:F$digits}", $num)
}

function FmtP([object]$value) {
    if ($null -eq $value -or $value -eq '') { return 'NA' }
    $num = [double]$value
    if ($num -lt 0.001) { return '<0.001' }
    return [string]::Format([System.Globalization.CultureInfo]::InvariantCulture, "{0:F3}", $num)
}

function FmtPClause([object]$value) {
    $p = FmtP $value
    if ($p -eq 'NA') { return 'p = NA' }
    if ($p.StartsWith('<')) { return "p $p" }
    return "p = $p"
}

$timeRow = Main-Row 'time'
$distanceRow = Main-Row 'distance'
$dxRow = Main-Row 'dx'
$vxhRow = Main-Row 'vxh'
$sjRow = Main-Row 'sj'
$cmjRow = Main-Row 'cmj'
$sljRow = Main-Row 'slj'
$fmsTotalRow = Main-Row 'fms_total'
$fmsHurdleRRow = Main-Row 'fms_hurdle_r'
$fmsHurdleLRow = Main-Row 'fms_hurdle_l'
$fmsLungeRRow = Main-Row 'fms_lunge_r'
$lowerLimbRowsData = $main | Where-Object { $_.outcome -in @('sj','cmj','slj') }
$startRowsData = $main | Where-Object { $_.outcome -in @('time','distance','dx','vxh') }
$fmsRowsData = $main | Where-Object { $_.domain -eq 'FMS' }

$unitMap = @{
    'time' = 's'
    'distance' = 'cm'
    'dx' = 'cm'
    'dt' = 's'
    'vxh' = 'm/s'
    'cmj' = 'cm'
    'sj' = 'cm'
    'slj' = 'cm'
    'fms_total' = 'score'
    'fms_deep_squat' = 'score'
    'fms_hurdle_r' = 'score'
    'fms_hurdle_l' = 'score'
    'fms_lunge_l' = 'score'
    'fms_lunge_r' = 'score'
    'fms_shoulder_l' = 'score'
    'fms_shoulder_r' = 'score'
    'fms_aslr_l' = 'score'
    'fms_aslr_r' = 'score'
    'fms_trunk_push_up' = 'score'
    'fms_rotary_l' = 'score'
    'fms_rotary_r' = 'score'
}

$ethicsLine = 'Ethics approval and consent to participate: approved by the Ethics Committee of Chengdu Sport University, Sichuan, China (approval code [ETHICS APPROVAL CODE TO BE INSERTED]). Participants provided assent, and written informed consent was obtained from their parents or legal guardians before participation.'
$ethicsParagraph = 'This study was approved by the Ethics Committee of Chengdu Sport University, Sichuan, China (approval code [ETHICS APPROVAL CODE TO BE INSERTED]). All procedures were conducted in accordance with the Declaration of Helsinki.'
$consentParagraph = 'Participants provided assent, and written informed consent was obtained from their parents or legal guardians before participation in the study.'

$abstractMethods = 'Methods: Seventeen competitive adolescent swimmers (age: 10.5 +/- 1.4 years) were randomly assigned to either an INT group (INTG, n = 9) or a conventional training group (CON, n = 8). The INTG performed a progressive INT program twice weekly for 8 weeks, whereas the CON completed traditional strength and conditioning over the same period. Outcomes included the standard 7-item Functional Movement Screen total score (0-21), individual FMS task scores, squat jump, countermovement jump, standing long jump, and swimming-start variables. The primary analysis was a 2 x 2 mixed repeated-measures ANOVA with Group (CON vs INTG) and Visit (Pre vs Post), with the Group x Visit interaction treated as the primary effect of interest. Holm-adjusted post hoc comparisons and ANCOVA sensitivity analyses (Post ~ Group + Pre) were performed when appropriate.'
$abstractResults = 'Results: Significant Group x Visit interactions favored INTG for 15-m sprint time, water-entry distance, horizontal displacement (Delta x), horizontal velocity, FMS Total (0-21), FMS Hurdle Step (right and left), and FMS In-line Lunge (right) (all p < 0.05). ANCOVA sensitivity analyses supported these findings after baseline adjustment. No significant group-by-visit interactions were observed for the generic lower-limb explosive power outcomes.'

$participantsText = 'Seventeen competitive adolescent swimmers (6 males, 11 females; age: 10.5 +/- 1.4 years; height: 149.0 +/- 13.5 cm; body mass: 37.9 +/- 10.7 kg) were recruited from DY Olympic School on 31 March 2025, a government-supported adolescent training department. All participants met the following inclusion criteria: (i) a minimum of one year of systematic swimming training; (ii) classification as Tier 2 (developmental athletes) according to the participant classification framework; (iii) proficiency in all four competitive swimming strokes; and (iv) regular supervised aquatic training with a frequency of no fewer than three weekly sessions under the same head coach. Athletes were free from musculoskeletal, neurological, or orthopedic disorders within the previous 6 months and did not take nutritional supplements or medications known to affect physical performance. Sample-size planning was aligned to the final mixed repeated-measures ANOVA framework. Using G*Power (version 3.1.9; Heinrich Heine University, Duesseldorf, Germany), the unified settings were F tests, ANOVA: repeated measures, within-between interaction, with alpha = 0.05, power = 0.80, two groups, two measurements, and a medium effect size (f = 0.25). The 15-m sprint time outcome was used as the representative primary endpoint because it most directly reflects swimming-start performance. Because the accessible competitive cohort was limited, all eligible swimmers were enrolled, resulting in a final sample of 17 participants. The protocol was approved by the Ethics Committee of Chengdu Sport University, Sichuan, China (approval code [ETHICS APPROVAL CODE TO BE INSERTED]). Participants provided assent, and written informed consent was obtained from their parents or legal guardians before participation.'

$statsPara1 = 'All data are presented as mean (SD) for continuous variables or n (%) for categorical variables. Pre- and post-intervention records were matched one-to-one using participant number, participant name, and group after standardization of spacing, character width, letter case, and group coding. For FMS, the standard total score (0-21) was derived using the 7-item scoring algorithm by summing Deep Squat, Trunk Stability Push-Up, and the lower score from each bilateral task. The primary analytic framework was a 2 x 2 mixed repeated-measures ANOVA with Group (CON vs INTG) as the between-subject factor and Visit (Pre vs Post) as the within-subject factor. The Group x Visit interaction was treated as the primary effect of interest. For each outcome, F values, degrees of freedom, p values, and partial eta squared were reported.'
$statsPara2 = 'Holm-adjusted post hoc comparisons were conducted only when the interaction effect was significant or when additional interpretation of the trajectory was required, including within-group Pre vs Post contrasts and between-group contrasts at each visit. Within-group pre-to-post standardized changes were additionally summarized using Cohen''s d based on the pooled pre/post standard deviation. Assumption checks included normality and homogeneity assessments, and potential outliers were screened using studentized residuals. ANCOVA models specified as Post ~ Group + Pre were performed as sensitivity analyses to evaluate whether the mixed ANOVA findings remained consistent after baseline adjustment. All analyses were performed in R (version 4.5.2; R Foundation for Statistical Computing, Vienna, Austria), and statistical significance was set at p < 0.05.'

$baselineResults = 'No sports-related injuries occurred during testing or training. Training compliance was high in both groups (INTG: 95.5%; CON: 96.0%), and all 17 participants completed the intervention (INTG: n = 9; CON: n = 8). All 21 prespecified outcomes had complete paired values, and no missing outcome data were identified; therefore, all analyses were conducted on complete cases. Baseline characteristics and pre-intervention performance measures are summarized in Table 3. No statistically significant between-group differences were observed at baseline for participant characteristics, lower-limb explosive performance, start performance, or FMS Total (all p > 0.05). Assumption checks and outlier screening are reported in the Supplementary Materials.'

$lowerLimbPara = "The lower-limb explosive power findings are included in Table 4 and Figure 1. No statistically significant Group x Visit interactions were observed for squat jump (F(1,15) = $(FmtNum $sjRow.interaction_f 2), p = $(FmtP $sjRow.interaction_p), partial eta squared = $(FmtNum $sjRow.interaction_pes 3)), countermovement jump (F(1,15) = $(FmtNum $cmjRow.interaction_f 2), p = $(FmtP $cmjRow.interaction_p), partial eta squared = $(FmtNum $cmjRow.interaction_pes 3)), or standing long jump (F(1,15) = $(FmtNum $sljRow.interaction_f 2), p = $(FmtP $sljRow.interaction_p), partial eta squared = $(FmtNum $sljRow.interaction_pes 3)). Changes over time therefore did not differ significantly between INTG and CON. Detailed main-effect and post hoc results are reported in the Supplementary Materials."

$performancePara1 = "The primary start-performance and kinematic findings are summarized in Table 5 and Figure 2. Significant Group x Visit interactions were observed for water-entry distance (F(1,15) = $(FmtNum $distanceRow.interaction_f 2), p = $(FmtP $distanceRow.interaction_p), partial eta squared = $(FmtNum $distanceRow.interaction_pes 3)), horizontal velocity (F(1,15) = $(FmtNum $vxhRow.interaction_f 2), p = $(FmtP $vxhRow.interaction_p), partial eta squared = $(FmtNum $vxhRow.interaction_pes 3)), 15-m sprint time (F(1,15) = $(FmtNum $timeRow.interaction_f 2), p = $(FmtP $timeRow.interaction_p), partial eta squared = $(FmtNum $timeRow.interaction_pes 3)), and horizontal displacement (Delta x) (F(1,15) = $(FmtNum $dxRow.interaction_f 2), p = $(FmtP $dxRow.interaction_p), partial eta squared = $(FmtNum $dxRow.interaction_pes 3)). These interaction effects were in favor of INTG."
$performancePara2 = "INTG reduced 15-m sprint time from $($timeRow.INTG_Pre) s to $($timeRow.INTG_Post) s, whereas CON changed from $($timeRow.CON_Pre) s to $($timeRow.CON_Post) s. Water-entry distance increased from $($distanceRow.INTG_Pre) cm to $($distanceRow.INTG_Post) cm in INTG, while the corresponding change in CON was small. A comparable pattern was observed for the kinematic outcomes: INTG showed larger gains in horizontal displacement ($($dxRow.INTG_Pre) cm to $($dxRow.INTG_Post) cm) and horizontal velocity ($($vxhRow.INTG_Pre) m/s to $($vxhRow.INTG_Post) m/s), whereas CON displayed only modest displacement changes together with a decline in horizontal velocity. Holm-adjusted post hoc comparisons showed significant pre-to-post improvements in INTG for the four primary start-performance outcomes, whereas the clearest post-test between-group separation was observed for horizontal velocity. Figure 2 displays, from left to right within each group cell, paired trajectories, boxplots, and half-violin summaries for the Pre and Post distributions; inferential conclusions were based on the mixed repeated-measures ANOVA rather than on the visual summaries alone."

$fmsPara1 = "The functional movement findings are presented in Table 6 and Figure 3. Significant Group x Visit interactions were found for FMS Total (0-21) (F(1,15) = $(FmtNum $fmsTotalRow.interaction_f 2), p = $(FmtP $fmsTotalRow.interaction_p), partial eta squared = $(FmtNum $fmsTotalRow.interaction_pes 3)), FMS Hurdle Step, right (F(1,15) = $(FmtNum $fmsHurdleRRow.interaction_f 2), p = $(FmtP $fmsHurdleRRow.interaction_p), partial eta squared = $(FmtNum $fmsHurdleRRow.interaction_pes 3)), FMS Hurdle Step, left (F(1,15) = $(FmtNum $fmsHurdleLRow.interaction_f 2), p = $(FmtP $fmsHurdleLRow.interaction_p), partial eta squared = $(FmtNum $fmsHurdleLRow.interaction_pes 3)), and FMS In-line Lunge, right (F(1,15) = $(FmtNum $fmsLungeRRow.interaction_f 2), p = $(FmtP $fmsLungeRRow.interaction_p), partial eta squared = $(FmtNum $fmsLungeRRow.interaction_pes 3)). The pattern of change was more favorable in INTG than in CON."
$fmsPara2 = "The standard FMS Total score increased from $($fmsTotalRow.INTG_Pre) to $($fmsTotalRow.INTG_Post) in INTG, whereas CON changed from $($fmsTotalRow.CON_Pre) to $($fmsTotalRow.CON_Post). Improvements were also apparent in unilateral control tasks, including FMS Hurdle Step, right ($($fmsHurdleRRow.INTG_Pre) to $($fmsHurdleRRow.INTG_Post)), FMS Hurdle Step, left ($($fmsHurdleLRow.INTG_Pre) to $($fmsHurdleLRow.INTG_Post)), and FMS In-line Lunge, right ($($fmsLungeRRow.INTG_Pre) to $($fmsLungeRRow.INTG_Post)). Only the outcomes with significant interaction effects are described in detail here; the full FMS panel is provided in Table 6."

$sensitivityPara = "The ANCOVA sensitivity analyses, specified as post-test outcome ~ group + pre-test outcome, were consistent with the primary mixed-ANOVA results (Figure 4). After baseline adjustment, significant group effects remained evident for 15-m sprint time ($(FmtPClause $timeRow.ancova_group_p)), water-entry distance ($(FmtPClause $distanceRow.ancova_group_p)), horizontal displacement (Delta x) ($(FmtPClause $dxRow.ancova_group_p)), horizontal velocity ($(FmtPClause $vxhRow.ancova_group_p)), and FMS Total (0-21) ($(FmtPClause $fmsTotalRow.ancova_group_p)). The adjusted estimates remained in the same direction as the primary interaction-based analyses."

$supplementaryPara = 'The remaining performance and FMS variables are reported in the Supplementary Materials. These outputs include the full mixed ANOVA results for all 21 outcomes, Holm-adjusted post hoc comparisons, assumption checks, outlier screening, and ANCOVA models. To maintain a focused Results section, these secondary and exploratory findings are not described in detail here.'

$discussionPurpose = 'The purpose of this study was to examine the effects of an 8-week integrated neuromuscular training (INT) program on start performance, functional movement quality, and lower-limb explosive performance in competitive swimmers. The main findings indicate that INT produced more favorable pre-to-post trajectories than conventional training for several start-performance and movement-quality outcomes. Significant Group x Visit interactions were observed for 15-m sprint time, water-entry distance, horizontal displacement (Delta x), horizontal velocity, FMS Total (0-21), FMS Hurdle Step (right and left), and FMS In-line Lunge (right), whereas no significant group-by-visit interactions were found for the generic lower-limb explosive power outcomes. The ANCOVA sensitivity analyses supported the same pattern after baseline adjustment. Together, these findings suggest that INT improved the transfer of dry-land neuromuscular adaptations to swimming-start performance and functional movement quality rather than producing broad between-group differences across all jump-based measures.'
$discussionResults = 'Our results demonstrate that the 8-week INT intervention conferred substantial advantages in the trajectory of start-specific performance. Relative to CON, INTG showed a more favorable pre-to-post pattern for 15-m sprint time, water-entry distance, horizontal displacement, and horizontal velocity, with significant Group x Visit interactions across these outcomes. Post hoc contrasts indicated clear within-group improvements in INTG, whereas CON showed either smaller changes or changes in the opposite direction. Thus, the primary evidence in favor of INT was the differential change over time rather than isolated post-test comparisons.'
$conclusionText = 'This study demonstrates that an 8-week integrated neuromuscular training (INT) program improved start-related performance and movement quality in youth swimmers. Compared with conventional training, INT produced more favorable pre-to-post changes in 15-m sprint time, water-entry distance, horizontal displacement, horizontal velocity, and selected FMS outcomes, while generic lower-limb explosive power outcomes did not show significant group-by-visit interactions. These findings suggest that INT is an effective dry-land strategy for improving the movement quality and swimming-start mechanics of young swimmers.'
$startMethodText = 'Start performance was evaluated using 15-m sprint time, water-entry distance, and horizontal velocity. The 15-m sprint time test was conducted in a standard 50 m indoor swimming pool with water temperature maintained at 27 C. All participants used a grab start and were instructed to perform the start, water entry, and initial acceleration phase with maximal effort following the starting signal. Timing began at the start signal and ended when the participant''s head crossed the vertical plane of the 15 m mark. Performance was recorded using an automatic timing system. Each participant completed two trials separated by a 10-min rest period, and the best time was used for statistical analysis.'
$kinematicsText = 'To further examine the biomechanical mechanisms underlying differences in start performance, two-dimensional kinematic analysis was used to quantify key parameters during the take-off and flight phase. Video data were captured using a high-speed camera (GoPro 7; GoPro Inc., USA) operating at a sampling frequency of 120 Hz and a resolution of 1080p, positioned at poolside 5 m from the centerline of the starting block and at a height of 120 cm. The optical axis was aligned perpendicular to the pool''s longitudinal axis to ensure full capture of the movement from take-off to complete water entry (Supplementary Figure S1). All videos were processed in Kinovea to calculate horizontal displacement (Delta x) and flight-time difference (Delta t) from toe-off to initial head contact with the water, from which horizontal velocity was derived(31). To minimize the influence of fatigue, all in-water tests were performed 48 h after the final dry-land test session and were supervised by the same experienced swimming coach to ensure consistency of testing procedures and technique.'
$discussionBiomech = 'From a biomechanical perspective, the swimming start represents a complex multi-joint movement requiring the rapid generation of horizontal impulse within a severely constrained temporal window. Unlike vertical jumping activities where ground contact duration permits progressive force development, the swimming start demands near-maximal force production within approximately 300-400 milliseconds of block contact, followed immediately by aerial transition(29,35). These specific demands are highly consistent with the principle of force-vector specificity highlighted in recent literature. A meta-analysis showed that horizontally oriented plyometric exercises are more effective at enhancing forward acceleration than vertical exercises(36). Specifically, an 8-week intervention demonstrated that horizontal-vector resistance training yielded superior improvements in swim-start performance compared with traditional vertical-vector training(37). This helps explain the divergent adaptations observed in our study. By incorporating horizontally oriented stretch-shortening-cycle activities, the INT program more closely matched the mechanical demands of the starting block, contributing to the increases in horizontal velocity and water-entry distance. In contrast, the CON program relied primarily on exercises that emphasized general maximal strength and therefore showed less transfer to the specific demands of the start.'

try {
    Replace-ParagraphText 'Ethics approval and consent to participate:' $ethicsLine
    Replace-ParagraphText 'This study was approved by the Ethics Committee of Chengdu Sports University.' $ethicsParagraph
    Replace-ParagraphText 'Methods:' $abstractMethods
    Replace-ParagraphText 'Results:' $abstractResults

    $participantsPos = Delete-RangeBetweenHeadings '2.1 Participants' '2.2 Integrated Neuromuscular Training Testing Procedures'
    $sel.SetRange([int]$participantsPos, [int]$participantsPos)
    Add-RedParagraph $participantsText

    Replace-ParagraphText 'Start performance was evaluated using three outcomes:' $startMethodText
    Replace-ParagraphText 'To further examine the biomechanical mechanisms underlying differences in start performance' $kinematicsText
    Replace-ParagraphText 'Figure 1 Schematic diagram of the experimental swimming lane configuration' 'Supplementary Figure S1. Schematic diagram of the experimental swimming lane configuration.'
    $doc.Save()

    $statsPos = Delete-RangeBetweenHeadings '2.4 Statistical analysis' '3. Results'
    $sel.SetRange([int]$statsPos, [int]$statsPos)
    Add-RedParagraph $statsPara1
    Add-RedParagraph $statsPara2
    $doc.Save()

    $resultsPos = Delete-RangeBetweenHeadings '3. Results' '4. Discussion'
    $sel.SetRange([int]$resultsPos, [int]$resultsPos)
    Add-RedParagraph '3.1 Participant flow and baseline characteristics' $wdStyleHeading2
    Add-RedParagraph $baselineResults
    Add-RedParagraph 'Table 3. Baseline characteristics and pre-intervention performance variables.'
    $baselineRows = @()
    $baselineRows += ,@('Variable','INTG (n = 9)','CON (n = 8)','P')
    foreach ($row in $baseline) {
        $baselineRows += ,@($row.'Variable', $row.'INTG', $row.'CON', $row.'P')
    }
    Add-RedTable $baselineRows
    Add-RedParagraph 'Note. Data are presented as mean (SD) or n (%). Baseline p values are descriptive rather than decisive evidence of comparability. BMI = body mass index; SJ = squat jump; CMJ = countermovement jump; SLJ = standing long jump; FMS = Functional Movement Screen.'

    Add-RedParagraph '3.2 Lower-limb explosive power outcomes' $wdStyleHeading2
    Add-RedParagraph $lowerLimbPara

    Add-RedParagraph '3.3 Start-performance and kinematic outcomes' $wdStyleHeading2
    Add-RedParagraph $performancePara1
    Add-RedParagraph $performancePara2
    Add-RedParagraph 'Table 4. Lower-limb explosive power outcomes from the mixed repeated-measures ANOVA.'
    $lowerRows = @()
    $lowerRows += ,@('Outcome','Metric','CON','INTG','Group x Visit P','Partial eta squared')
    foreach ($row in $table4Display) {
        $lowerRows += ,@(
            $row.Outcome,
            $row.Metric,
            $row.CON,
            $row.INTG,
            $row.'Group x Visit P',
            $row.'Partial eta squared'
        )
    }
    Add-RedTable $lowerRows
    Add-RedParagraph 'Note. Data are presented as mean (SD). Post-pre difference (%) was calculated as ((Post - Pre) / Pre) x 100. Post-pre ES (d) represents the within-group standardized change from pre to post, calculated as (Post - Pre) / sqrt((SD_pre^2 + SD_post^2)/2). Cohen''s d magnitudes were interpreted as trivial for |d| < 0.20, small for 0.20-0.49, moderate for 0.50-0.79, and large for >= 0.80. The primary inference is the Group x Visit interaction from the mixed repeated-measures ANOVA. * p < 0.05, ** p < 0.01, and *** p < 0.001 for Holm-adjusted within-group Pre vs Post comparisons; # indicates a Holm-adjusted between-group difference at Post. Full main effects, change scores, Holm-adjusted post hoc p values, and ANCOVA outputs are reported in the Supplementary Materials. SJ = squat jump; CMJ = countermovement jump; SLJ = standing long jump.'
    Add-RedParagraph 'Table 5. Start-performance and kinematic outcomes from the mixed repeated-measures ANOVA.'
    $startRows = @()
    $startRows += ,@('Outcome','Metric','CON','INTG','Group x Visit P','Partial eta squared')
    foreach ($row in $table5Display) {
        $startRows += ,@(
            $row.Outcome,
            $row.Metric,
            $row.CON,
            $row.INTG,
            $row.'Group x Visit P',
            $row.'Partial eta squared'
        )
    }
    Add-RedTable $startRows
    Add-RedParagraph 'Note. Data are presented as mean (SD). Post-pre difference (%) was calculated as ((Post - Pre) / Pre) x 100. Post-pre ES (d) represents the within-group standardized change from pre to post, calculated as (Post - Pre) / sqrt((SD_pre^2 + SD_post^2)/2). Cohen''s d magnitudes were interpreted as trivial for |d| < 0.20, small for 0.20-0.49, moderate for 0.50-0.79, and large for >= 0.80. The primary inference is the Group x Visit interaction from the mixed repeated-measures ANOVA. * p < 0.05, ** p < 0.01, and *** p < 0.001 for Holm-adjusted within-group Pre vs Post comparisons; # indicates a Holm-adjusted between-group difference at Post. Full main effects, change scores, Holm-adjusted post hoc p values, and ANCOVA outputs are reported in the Supplementary Materials.'
    Add-RedParagraph 'Figure 1. Lower-limb explosive power outcomes arranged by rows and groups by columns. Within each group cell, paired trajectories, boxplots, and half-violin distributions are displayed from left to right for the Pre and Post measurements. ES annotations are displayed above the trajectory panels, and asterisks denote significance levels based on Holm-adjusted within-group Pre vs Post comparisons. Inferential conclusions were based on the mixed repeated-measures ANOVA.'
    Add-CenteredPicture 'figures\lower_limb_main.png' 470
    Add-RedParagraph 'Figure 2. Start-performance outcomes arranged by rows and groups by columns. Within each group cell, paired trajectories, boxplots, and half-violin distributions are displayed from left to right for the Pre and Post measurements. ES annotations are displayed above the trajectory panels, and asterisks denote significance levels based on Holm-adjusted within-group Pre vs Post comparisons. Horizontal displacement (Delta x) and Delta t are presented in the Supplementary Materials, and inferential conclusions were based on the mixed repeated-measures ANOVA.'
    Add-CenteredPicture 'figures\start_main.png' 470

    Add-RedParagraph '3.4 Functional movement outcomes' $wdStyleHeading2
    Add-RedParagraph $fmsPara1
    Add-RedParagraph $fmsPara2
    Add-RedParagraph 'Table 6. Functional movement outcomes from the mixed repeated-measures ANOVA.'
    $fmsRows = @()
    $fmsRows += ,@('Outcome','Metric','CON','INTG','Group x Visit P','Partial eta squared')
    foreach ($row in $table6Display) {
        $fmsRows += ,@(
            $row.Outcome,
            $row.Metric,
            $row.CON,
            $row.INTG,
            $row.'Group x Visit P',
            $row.'Partial eta squared'
        )
    }
    Add-RedTable $fmsRows
    Add-RedParagraph 'Note. Data are presented as mean (SD). Post-pre difference (%) was calculated as ((Post - Pre) / Pre) x 100. Post-pre ES (d) represents the within-group standardized change from pre to post, calculated as (Post - Pre) / sqrt((SD_pre^2 + SD_post^2)/2). Cohen''s d magnitudes were interpreted as trivial for |d| < 0.20, small for 0.20-0.49, moderate for 0.50-0.79, and large for >= 0.80. The primary inference is the Group x Visit interaction from the mixed repeated-measures ANOVA. * p < 0.05, ** p < 0.01, and *** p < 0.001 for Holm-adjusted within-group Pre vs Post comparisons; # indicates a Holm-adjusted between-group difference at Post. Detailed change scores, Holm-adjusted post hoc p values, and ANCOVA outputs are reported in the Supplementary Materials. FMS = Functional Movement Screen.'
    Add-RedParagraph 'Figure 3. FMS outcomes with statistically significant Group x Visit interactions displayed in the same row-by-column layout as Figures 1 and 2. Within each group cell, paired trajectories, boxplots, and half-violin distributions are displayed from left to right for the Pre and Post measurements. ES annotations are displayed above the trajectory panels, and asterisks denote significance levels based on Holm-adjusted within-group Pre vs Post comparisons. Inferential conclusions were based on the mixed repeated-measures ANOVA.'
    Add-CenteredPicture 'figures\fms_main.png' 450

    Add-RedParagraph '3.5 Sensitivity analyses' $wdStyleHeading2
    Add-RedParagraph $sensitivityPara
    Add-RedParagraph 'Figure 4. Baseline-adjusted confirmatory analyses for the selected performance outcomes and FMS Total. Positive standardized effects indicate a direction favoring INTG after baseline adjustment, horizontal lines denote 95% confidence intervals, and the estimates were consistent with the primary mixed-ANOVA analyses.'
    Add-CenteredPicture 'figures\sensitivity_forest.png' 420

    Add-RedParagraph '3.6 Supplementary results' $wdStyleHeading2
    Add-RedParagraph $supplementaryPara
    $doc.Save()

    Replace-ParagraphText 'The purpose of this study was to examine the effects of an 8-week integrated neuromuscular training (INT) program on start performance, functional movement quality, and lower-limb explosive performance in competitive swimmers.' $discussionPurpose
    Replace-ParagraphText 'Our results demonstrate that the 8-week INT intervention conferred substantial advantages in enhancing start-specific performance in competitive swimmers.' $discussionResults
    Replace-ParagraphText 'From a biomechanical perspective' $discussionBiomech
    Replace-ParagraphText 'This study demonstrates that an 8-week integrated neuromuscular training' $conclusionText

    Replace-ParagraphText 'Ethical Approval' 'Ethical Approval'
    Replace-ParagraphText 'This study was approved by the Ethics Committee of Chengdu Sport University.' $ethicsParagraph
    Replace-ParagraphText 'Consent to Participate' 'Consent to Participate'
    Replace-ParagraphText 'Written informed consent was obtained from all participants prior to their participation in the study.' $consentParagraph
    Replace-ParagraphText 'This study was approved by the Ethics Committee of Chengdu Sport University, Sichuan, China, and all procedures were conducted in accordance with the principles of the Declaration of Helsinki.' $ethicsParagraph
    Replace-ParagraphText 'Because the participants were adolescents, written informed consent was obtained from all participants and their parents or legal guardians prior to participation in the study.' $consentParagraph
    $doc.Save()
}
finally {
    if ($doc -ne $null) { $doc.Close() }
    if ($word -ne $null) { try { $word.Quit() } catch {} }
}

Copy-Item -LiteralPath $workingName -Destination $outputName -Force
Write-Output (Resolve-Path $outputName).Path
