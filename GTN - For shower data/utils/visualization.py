import matplotlib.pyplot as plt
from matplotlib.font_manager import FontProperties as fp  
import math
def result_visualization(loss_list: list,
                         correct_on_test: list,
                         correct_on_train: list,
                         test_interval: int,
                         d_model: int,
                         q: int,
                         v: int,
                         h: int,
                         N: int,
                         dropout: float,
                         DATA_LEN: int,
                         BATCH_SIZE: int,
                         time_cost: float,
                         EPOCH: int,
                         draw_key: int,
                         reslut_figure_path: str,
                         optimizer_name: str,
                         file_name: str,
                         LR: float,
                         pe: bool,
                         mask: bool):
    my_font = fp(fname=r"font/simsun.ttc") 
    plt.style.use('seaborn-v0_8')

    fig = plt.figure()  
    ax1 = fig.add_subplot(311)  
    ax2 = fig.add_subplot(313)

    ax1.plot(loss_list)  
    ax2.plot(correct_on_test, color='red', label='on Test Dataset')
    ax2.plot(correct_on_train, color='blue', label='on Train Dataset')

    ax1.set_xlabel('epoch')
    ax1.set_ylabel('loss')
    ax2.set_xlabel(f'epoch/{test_interval}') 
    ax2.set_ylabel('correct')
    ax1.set_title('LOSS')
    ax2.set_title('CORRECT')

    plt.legend(loc='best')

    fig.text(x=0.13, y=0.4, s=f'loss：{min(loss_list)}' '    '
                              f'epoch:{math.ceil((loss_list.index(min(loss_list)) + 1) / math.ceil((DATA_LEN / BATCH_SIZE)))}' '    '
                              f'loss:{loss_list[-1]}' '\n'
                              f'correct:{max(correct_on_test)}% :{max(correct_on_train)}%' '    '
                              f'correct:{(correct_on_test.index(max(correct_on_test)) + 1) * test_interval}' '    '
                              f'correct：{correct_on_test[-1]}%' '\n'
                              f'd_model={d_model}   q={q}   v={v}   h={h}   N={N}  drop_out={dropout}'  '\n'
                              f'{round(time_cost, 2)}')

    
    if EPOCH >= draw_key:
        plt.savefig(
            f'{reslut_figure_path}/{file_name} {max(correct_on_test)}% {optimizer_name} epoch={EPOCH} batch={BATCH_SIZE} lr={LR} pe={pe} mask={mask} [{d_model},{q},{v},{h},{N},{dropout}].png')

    plt.show()
