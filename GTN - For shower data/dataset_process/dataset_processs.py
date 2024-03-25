from torch.utils.data import Dataset
import numpy as np
import torch

from scipy.io import loadmat


class MyDataset(Dataset):
    def __init__(self,
                 path: str,
                 dataset: str): 
        super(MyDataset, self).__init__()
        self.dataset = dataset  
        self.train_len, \
        self.test_len, \
        self.input_len, \
        self.channel_len, \
        self.output_len, \
        self.train_dataset, \
        self.train_label, \
        self.test_dataset, \
        self.test_label, \
        self.max_length_sample_inTest, \
        self.train_dataset_with_no_paddding = self.pre_option(path)

    def __getitem__(self, index):
        if self.dataset == 'train':
            return self.train_dataset[index], self.train_label[index] - 1
        elif self.dataset == 'test':
            return self.test_dataset[index], self.test_label[index] - 1

    def __len__(self):
        if self.dataset == 'train':
            return self.train_len
        elif self.dataset == 'test':
            return self.test_len

    def pre_option(self, index):
        train_data2 = np.load("C:\\Users\\lukin\\code\\GTN\\Gated Transformer 论文IJCAI版\\data\\Shower\\X_train.npy")
        train_label2 = np.load("C:\\Users\\lukin\\code\\GTN\\Gated Transformer 论文IJCAI版\\data\\Shower\\Y_train.npy").astype(np.uint8)
        test_data2 = np.load("C:\\Users\\lukin\\code\\GTN\\Gated Transformer 论文IJCAI版\\data\\Shower\\X_test.npy")
        test_label2 = np.load("C:\\Users\\lukin\\code\\GTN\\Gated Transformer 论文IJCAI版\\data\\Shower\\Y_test.npy").astype(np.uint8)

        train_data2 = train_data2[:,index,:]
        test_data2 = test_data2[:,index,:]


        train_len = train_data2.shape[0]
        test_len = test_data2.shape[0]
        output_len = len(tuple(set(train_label2)))

        max_lenth = 25  # 93

        # Padding  
        train_dataset_with_no_paddding = []
        test_dataset_with_no_paddding = []
        train_dataset = []
        test_dataset = []
        max_length_sample_inTest = []
        for x1 in train_data2:
            train_dataset_with_no_paddding.append(x1.transpose(-1, -2).tolist())
            x1 = torch.as_tensor(x1).float()
            if x1.shape[1] != max_lenth:
                padding = torch.zeros(x1.shape[0], max_lenth - x1.shape[1])
                x1 = torch.cat((x1, padding), dim=1)
            train_dataset.append(x1)

        for index, x2 in enumerate(test_data2):
            test_dataset_with_no_paddding.append(x2.transpose(-1, -2).tolist())
            x2 = torch.as_tensor(x2).float()
            if x2.shape[1] != max_lenth:
                padding = torch.zeros(x2.shape[0], max_lenth - x2.shape[1])
                x2 = torch.cat((x2, padding), dim=1)
            else:
                max_length_sample_inTest.append(x2.transpose(-1, -2))
            test_dataset.append(x2)

        # train_dataset_with_no_paddding = torch.stack(train_dataset_with_no_paddding, dim=0).permute(0, 2, 1)
        # test_dataset_with_no_paddding = torch.stack(test_dataset_with_no_paddding, dim=0).permute(0, 2, 1)
        train_dataset = torch.stack(train_dataset, dim=0).permute(0, 2, 1)
        test_dataset = torch.stack(test_dataset, dim=0).permute(0, 2, 1)
        train_label = torch.Tensor(train_label2)
        test_label = torch.Tensor(test_label2)
        channel = test_dataset[0].shape[-1]
        input = test_dataset[0].shape[-2]

        return train_len, test_len, input, channel, output_len, train_dataset, train_label, test_dataset, test_label, max_length_sample_inTest, train_dataset_with_no_paddding